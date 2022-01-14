package com.teenthofabud.restaurant.solution.cookbook.ingredient.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientEntity;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.data.ProductVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.proxy.ProductServiceClient;
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
public class IngredientEntity2VoConverter extends TOABBaseEntity2VoConverter<IngredientEntity, IngredientVo> implements Converter<IngredientEntity, IngredientVo> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.cookbook.recipe.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private CookbookServiceHelper cookbookServiceHelper;
    private ProductServiceClient productServiceClient;

    @Autowired
    public void setCookbookServiceHelper(CookbookServiceHelper cookbookServiceHelper) {
        this.cookbookServiceHelper = cookbookServiceHelper;
    }

    @Autowired
    public void setProductServiceClient(ProductServiceClient productServiceClient) {
        this.productServiceClient = productServiceClient;
    }

    @Override
    public IngredientVo convert(IngredientEntity entity) {
        IngredientVo vo = new IngredientVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        if(!fieldsToEscape.contains("recipeId")) {
            this.expandSecondLevelFields(entity, vo, "recipeId");
        }
        if(!fieldsToEscape.contains("productId")) {
            this.expandSecondLevelFields(entity, vo, "productId");
        }
        if(!fieldsToEscape.contains("quantityAmount")) {
            String quantity = this.parseSpecification(entity.getQuantityAmount(), entity.getQuantityUnitId(), "quantityAmount", UnitDimension.MASS);
            vo.setQuantity(quantity);
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

    private String parseSpecification(Double amount, String unitId, String fieldName, Dimension unitType) {
        String amountInStr = !fieldsToEscape.contains(fieldName) ? amount.toString() : "";
        String unitName = "";
        if(unitType.equals(UnitDimension.TIME)) {
            unitName = cookbookServiceHelper.parseTimeId(unitId).get().getName();
        } else if(unitType.equals(UnitDimension.MASS)) {
            unitName = cookbookServiceHelper.parseWeightCode(unitId).get().getName();
        } else {
            log.debug("unit {} not supported", unitId);
            throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object [] { "unitId " + unitId + " not supported" });
        }
        String specificationValue = String.join(" ", Arrays.asList(amountInStr, unitName));
        specificationValue = StringUtils.hasText(StringUtils.trimWhitespace(specificationValue)) ? specificationValue : null;
        return specificationValue;
    }

    private void expandSecondLevelFields(IngredientEntity entity, IngredientVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("recipeId") && fieldName.compareTo("recipeId") == 0) {
                    Callable<RecipeVo> recipeEntity2VoConversion = () -> {
                        TOABRequestContextHolder.setCascadeLevelContext(TOABCascadeLevel.ZERO);
                        RecipeVo recipeVo = cookbookServiceHelper.recipeEntity2DetailedVo(entity.getRecipe());
                        TOABRequestContextHolder.clearCascadeLevelContext();
                        return recipeVo;
                    };
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory("recipeEntity2VoConversion-"));
                    Future<RecipeVo> recipeEntity2VoConversionResult = executorService.submit(recipeEntity2VoConversion);
                    try {
                        RecipeVo recipeVo = recipeEntity2VoConversionResult.get();
                        vo.setRecipe(recipeVo);
                        log.debug("Retrieved {} for recipeId: {}", recipeVo, entity.getRecipe().getId());
                    } catch (InterruptedException | ExecutionException e) {
                        log.error("Unable to perform recipeEntity2VoConversion", e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "Unable to perform recipeEntity2VoConversion",
                                new Object[] { "recipeEntity2VoConversion failure: " + e.getMessage() });
                    }
                }
                if(!fieldsToEscape.contains("productId") && fieldName.compareTo("productId") == 0) {
                    ProductVo product = productServiceClient.getProductDetailsById(entity.getProductId());
                    log.debug("Retrieved {} for product is: {}", product, entity.getProductId());
                    vo.setProduct(product);
                }
                break;
            default:
                vo.setRecipeId(entity.getRecipe().getId().toString());
                vo.setProductId(entity.getProductId());
                log.debug("only first level cascaded for ingredient over recipeId and productId");
                break;
        }
    }

}
