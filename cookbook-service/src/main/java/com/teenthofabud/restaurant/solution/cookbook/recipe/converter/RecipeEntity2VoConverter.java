package com.teenthofabud.restaurant.solution.cookbook.recipe.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.data.ItemVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.proxy.MenuServiceClient;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import com.teenthofabud.restaurant.solution.cookbook.utils.CookbookServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.scheduling.concurrent.CustomizableThreadFactory;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import tech.units.indriya.unit.UnitDimension;

import javax.measure.Dimension;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.*;

@Component
@Slf4j
public class RecipeEntity2VoConverter extends TOABBaseEntity2VoConverter<RecipeEntity, RecipeVo> implements Converter<RecipeEntity, RecipeVo> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.cookbook.recipe.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private CookbookServiceHelper cookbookServiceHelper;
    private MenuServiceClient menuServiceClient;

    @Autowired
    public void setCookbookServiceHelper(CookbookServiceHelper cookbookServiceHelper) {
        this.cookbookServiceHelper = cookbookServiceHelper;
    }

    @Autowired
    public void setItemServiceClient(MenuServiceClient menuServiceClient) {
        this.menuServiceClient = menuServiceClient;
    }

    @Override
    public RecipeVo convert(RecipeEntity entity) {
        RecipeVo vo = new RecipeVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("instructions")) {
            vo.setInstructions(entity.getInstructions());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        if(!fieldsToEscape.contains("cuisineId")) {
            this.expandSecondLevelFields(entity, vo, "cuisineId");
        }
        if(!fieldsToEscape.contains("itemId")) {
            this.expandSecondLevelFields(entity, vo, "itemId");
        }
        if(!fieldsToEscape.contains("cookingMethod")) {
            vo.setCookingMethod(entity.getCookingMethod());
        }
        if(!fieldsToEscape.contains("numberOfServings")) {
            vo.setNumberOfServings(entity.getNumberOfServings());
        }
        if(!fieldsToEscape.contains("preparationTimeDuration")) {
            String preparationTime = this.parseSpecification(entity.getPreparationTimeDuration(), entity.getPreparationTimeUnitId(), "preparationTimeDuration", UnitDimension.TIME);
            vo.setPreparationTime(preparationTime);
        }
        if(!fieldsToEscape.contains("cookingTimeDuration")) {
            String cookingTime = this.parseSpecification(entity.getCookingTimeDuration(), entity.getCookingTimeUnitId(), "cookingTimeDuration", UnitDimension.TIME);
            vo.setCookingTime(cookingTime);
        }
        if(!fieldsToEscape.contains("portionSizeAmount")) {
            String portionSize = this.parseSpecification(entity.getPortionSizeAmount(), entity.getPortionSizeUnitId(), "portionSizeAmount", UnitDimension.MASS);
            vo.setPortionSize(portionSize);
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

    private String parseSpecification(Double amount, String unitId, String fieldName, Dimension unitType) {
        String amountInStr = !fieldsToEscape.contains(fieldName) ? amount.toString() : "";
        String unitSymbol = "";
        if(unitType.equals(UnitDimension.TIME)) {
            unitSymbol = cookbookServiceHelper.parseTimeId(unitId).get().getSymbol();
        } else if(unitType.equals(UnitDimension.MASS)) {
            unitSymbol = cookbookServiceHelper.parseWeightCode(unitId).get().getSymbol();
        } else {
            log.debug("unit {} not supported", unitId);
            throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object [] { "unitId " + unitId + " not supported" });
        }
        String specificationValue = String.join(" ", Arrays.asList(amountInStr, unitSymbol));
        specificationValue = StringUtils.hasText(StringUtils.trimWhitespace(specificationValue)) ? specificationValue : null;
        return specificationValue;
    }

    private void expandSecondLevelFields(RecipeEntity entity, RecipeVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("cuisineId") && fieldName.compareTo("cuisineId") == 0) {
                    Callable<CuisineVo> cuisineEntity2VoConversion = () -> {
                        TOABRequestContextHolder.setCascadeLevelContext(TOABCascadeLevel.ZERO);
                        CuisineVo cuisineVo = cookbookServiceHelper.cuisineEntity2DetailedVo(entity.getCuisine());
                        TOABRequestContextHolder.clearCascadeLevelContext();
                        return cuisineVo;
                    };
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory("cuisineEntity2VoConversion-"));
                    Future<CuisineVo> cuisineEntity2VoConversionResult = executorService.submit(cuisineEntity2VoConversion);
                    try {
                        CuisineVo cuisineVo = cuisineEntity2VoConversionResult.get();
                        vo.setCuisine(cuisineVo);
                        log.debug("Retrieved {} for cuisineId: {}", cuisineVo, entity.getCuisine().getId());
                    } catch (InterruptedException | ExecutionException e) {
                        log.error("Unable to perform cuisineEntity2VoConversion", e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "Unable to perform cuisineEntity2VoConversion",
                                new Object[] { "cuisineEntity2VoConversion failure: " + e.getMessage() });
                    }
                }
                if(!fieldsToEscape.contains("itemId") && fieldName.compareTo("itemId") == 0) {
                    ItemVo item = menuServiceClient.getItemDetailsById(entity.getItemId());
                    log.debug("Retrieved {} for item is: {}", item, entity.getItemId());
                    vo.setItem(item);
                }
                break;
            default:
                vo.setCuisineId(entity.getCuisine().getId().toString());
                vo.setItemId(entity.getItemId());
                log.debug("only first level cascaded for recipe over cuisineId and itemId");
                break;
        }
    }

}
