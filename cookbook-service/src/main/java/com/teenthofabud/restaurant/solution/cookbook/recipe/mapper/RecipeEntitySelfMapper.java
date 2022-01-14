package com.teenthofabud.restaurant.solution.cookbook.recipe.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class RecipeEntitySelfMapper implements SingleChannelMapper<RecipeEntity> {

    @Override
    public Optional<RecipeEntity> compareAndMap(RecipeEntity source, RecipeEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source RecipeEntity.id is valid");
        }
        if(source.getName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getName())) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source RecipeEntity.name is valid");
        }
        if(source.getInstructions() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getInstructions())) && source.getInstructions().compareTo(target.getInstructions()) != 0) {
            target.setInstructions(source.getInstructions());
            changeSW = true;
            log.debug("Source RecipeEntity.instructions is valid");
        }
        if(source.getDescription() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getDescription())) && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source RecipeEntity.description is valid");
        }
        if(source.getItemId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getItemId())) && source.getItemId().compareTo(target.getItemId()) != 0) {
            target.setItemId(source.getItemId());
            changeSW = true;
            log.debug("Source RecipeEntity.itemId is valid");
        }
        if(source.getNumberOfServings() != null && source.getNumberOfServings() != null && source.getNumberOfServings().compareTo(target.getNumberOfServings()) != 0) {
            target.setNumberOfServings(source.getNumberOfServings());
            changeSW = true;
            log.debug("Source RecipeEntity.numberOfServings is valid");
        }
        if(source.getPreparationTimeDuration() != null && source.getPreparationTimeDuration() != null && source.getPreparationTimeDuration().compareTo(target.getPreparationTimeDuration()) != 0) {
            target.setPreparationTimeDuration(source.getPreparationTimeDuration());
            changeSW = true;
            log.debug("Source RecipeEntity.preparationTimeDuration is valid");
        }
        if(source.getPreparationTimeUnitId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getPreparationTimeUnitId())) && source.getPreparationTimeUnitId().compareTo(target.getPreparationTimeUnitId()) != 0) {
            target.setPreparationTimeUnitId(source.getPreparationTimeUnitId());
            changeSW = true;
            log.debug("Source RecipeEntity.preparationTimeUnitId is valid");
        }
        if(source.getCookingTimeDuration() != null && source.getCookingTimeDuration() != null && source.getCookingTimeDuration().compareTo(target.getCookingTimeDuration()) != 0) {
            target.setCookingTimeDuration(source.getCookingTimeDuration());
            changeSW = true;
            log.debug("Source RecipeEntity.cookingTimeDuration is valid");
        }
        if(source.getCookingTimeUnitId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getCookingTimeUnitId())) && source.getCookingTimeUnitId().compareTo(target.getCookingTimeUnitId()) != 0) {
            target.setCookingTimeUnitId(source.getCookingTimeUnitId());
            changeSW = true;
            log.debug("Source RecipeEntity.cookingTimeUnitId is valid");
        }
        if(source.getPortionSizeAmount() != null && source.getPortionSizeAmount() != null && source.getPortionSizeAmount().compareTo(target.getPortionSizeAmount()) != 0) {
            target.setPortionSizeAmount(source.getPortionSizeAmount());
            changeSW = true;
            log.debug("Source RecipeEntity.portionSizeAmount is valid");
        }
        if(source.getPortionSizeUnitId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getPortionSizeUnitId())) && source.getPortionSizeUnitId().compareTo(target.getPortionSizeUnitId()) != 0) {
            target.setPortionSizeUnitId(source.getPortionSizeUnitId());
            changeSW = true;
            log.debug("Source RecipeEntity.portionSizeUnitId is valid");
        }
        if(source.getCookingMethod() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getCookingMethod())) && source.getCookingMethod().compareTo(target.getCookingMethod()) != 0) {
            target.setCookingMethod(source.getCookingMethod());
            changeSW = true;
            log.debug("Source RecipeEntity.cookingMethod is valid");
        }
        if(source.getCuisine() != null && source.getCuisine().compareTo(target.getCuisine()) != 0) {
            target.setCuisine(source.getCuisine());
            changeSW = true;
            log.debug("Source RecipeEntity.cuisineId is valid");
        }
        if(changeSW) {
            log.debug("All provided RecipeEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided RecipeEntity attributes are valid");
            return Optional.empty();
        }
    }
}
