package com.teenthofabud.restaurant.solution.cookbook.ingredient.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class IngredientEntitySelfMapper implements SingleChannelMapper<IngredientEntity> {

    @Override
    public Optional<IngredientEntity> compareAndMap(IngredientEntity source, IngredientEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source IngredientEntity.id is valid");
        }
        if(source.getName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getName())) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source IngredientEntity.name is valid");
        }
        if(source.getDescription() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getDescription())) && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source IngredientEntity.description is valid");
        }
        if(source.getProductId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getProductId())) && source.getProductId().compareTo(target.getProductId()) != 0) {
            target.setProductId(source.getProductId());
            changeSW = true;
            log.debug("Source IngredientEntity.productId is valid");
        }
        if(source.getQuantityAmount() != null && source.getQuantityAmount() != null && source.getQuantityAmount().compareTo(target.getQuantityAmount()) != 0) {
            target.setQuantityAmount(source.getQuantityAmount());
            changeSW = true;
            log.debug("Source IngredientEntity.quantityAmount is valid");
        }
        if(source.getQuantityUnitId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getQuantityUnitId())) && source.getQuantityUnitId().compareTo(target.getQuantityUnitId()) != 0) {
            target.setQuantityUnitId(source.getQuantityUnitId());
            changeSW = true;
            log.debug("Source IngredientEntity.quantityUnitId is valid");
        }
        if(source.getRecipe() != null && source.getRecipe().compareTo(target.getRecipe()) != 0) {
            target.setRecipe(source.getRecipe());
            changeSW = true;
            log.debug("Source IngredientEntity.recipeId is valid");
        }
        if(changeSW) {
            log.debug("All provided IngredientEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided IngredientEntity attributes are valid");
            return Optional.empty();
        }
    }
}
