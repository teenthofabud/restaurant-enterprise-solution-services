package com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import com.teenthofabud.restaurant.solution.cookbook.utils.CookbookServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.scheduling.concurrent.CustomizableThreadFactory;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

@Component
@Slf4j
public class CuisineEntity2VoConverter extends TOABBaseEntity2VoConverter<CuisineEntity, CuisineVo> implements Converter<CuisineEntity, CuisineVo> {

    private List<String> fieldsToEscape;
    private CookbookServiceHelper cookbookServiceHelper;

    @Value("#{'${res.cookbook.cuisine.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setCuisineServiceHelper(CookbookServiceHelper cookbookServiceHelper) {
        this.cookbookServiceHelper = cookbookServiceHelper;
    }

    @Override
    public CuisineVo convert(CuisineEntity entity) {
        CuisineVo vo = new CuisineVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        if(!fieldsToEscape.contains("recipes")) {
            this.expandSecondLevelFields(entity, vo, "recipes");
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

    private void expandSecondLevelFields(CuisineEntity entity, CuisineVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("recipes") && fieldName.compareTo("recipes") == 0) {
                    Callable<List<RecipeVo>> recipeEntity2VoConversion = () -> {
                        TOABRequestContextHolder.setCascadeLevelContext(TOABCascadeLevel.ZERO);
                        List<RecipeEntity> recipeEntities = entity.getRecipes();
                        List<RecipeVo> recipeDetailsList = cookbookServiceHelper.recipeEntity2DetailedVo(recipeEntities);
                        TOABRequestContextHolder.clearCascadeLevelContext();
                        return recipeDetailsList;
                    };
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory("recipeEntity2VoConversion-"));
                    Future<List<RecipeVo>> recipeEntity2VoConversionResult = executorService.submit(recipeEntity2VoConversion);
                    try {
                        List<RecipeVo> recipeDetailsList = recipeEntity2VoConversionResult.get();
                        vo.setRecipes(recipeDetailsList);
                        log.debug("Retrieved {} recipees for cuisine id: {}", recipeDetailsList.size(), entity.getId());
                    } catch (InterruptedException | ExecutionException e) {
                        log.error("Unable to perform recipeEntity2VoConversion", e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "Unable to perform recipeEntity2VoConversion",
                                new Object[] { "recipeEntity2VoConversion failure: " + e.getMessage() });
                    }
                }
                break;
            default:
                break;
        }
    }

}
