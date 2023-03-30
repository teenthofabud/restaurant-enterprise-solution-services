package com.teenthofabud.restaurant.solution.cookbook.recipe.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.repository.CuisineJPARepository;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class RecipeForm2EntityMapper implements DualChannelMapper<RecipeEntity, RecipeForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.cookbook.recipe.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private CuisineJPARepository cuisineRepository;

    @Autowired
    public void setCuisineRepository(CuisineJPARepository cuisineRepository) {
        this.cuisineRepository = cuisineRepository;
    }

    @Override
    public Optional<RecipeEntity> compareAndMap(RecipeEntity actualEntity, RecipeForm form) {
        RecipeEntity expectedEntity = new RecipeEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying RecipeEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying RecipeEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying RecipeEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy

        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName()))
                && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("RecipeForm.name: {} is different as RecipeEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("RecipeForm.name: is unchanged");
        }

        if(!fieldsToEscape.contains("instructions") && StringUtils.hasText(StringUtils.trimWhitespace(form.getInstructions()))
                && form.getInstructions().compareTo(actualEntity.getInstructions()) != 0) {
            expectedEntity.setInstructions(form.getInstructions());
            changeSW = true;
            log.debug("RecipeForm.instructions: {} is different as RecipeEntity.instructions: {}", form.getInstructions(), actualEntity.getInstructions());
        } else {
            expectedEntity.setInstructions(actualEntity.getInstructions());
            log.debug("RecipeForm.instructions: is unchanged");
        }

        if(!fieldsToEscape.contains("description") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription()))
                && form.getDescription().compareTo(actualEntity.getDescription()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("RecipeForm.description: {} is different as RecipeEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            log.debug("RecipeForm.description: is unchanged");
        }

        if(!fieldsToEscape.contains("cookingMethod") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCookingMethod()))
                && form.getCookingMethod().compareTo(actualEntity.getCookingMethod()) != 0) {
            expectedEntity.setCookingMethod(form.getCookingMethod());
            changeSW = true;
            log.debug("RecipeForm.cookingMethod: {} is different as RecipeEntity.cookingMethod: {}", form.getCookingMethod(), actualEntity.getCookingMethod());
        } else {
            expectedEntity.setCookingMethod(actualEntity.getCookingMethod());
            log.debug("RecipeForm.cookingMethod: is unchanged");
        }

        if(!fieldsToEscape.contains("numberOfServings") && form.getNumberOfServings() != null && form.getNumberOfServings().compareTo(actualEntity.getNumberOfServings()) != 0) {
            expectedEntity.setNumberOfServings(form.getNumberOfServings());
            changeSW = true;
            log.debug("RecipeForm.numberOfServings: {} is different as RecipeEntity.numberOfServings: {}", form.getNumberOfServings(), actualEntity.getNumberOfServings());
        } else {
            expectedEntity.setNumberOfServings(actualEntity.getNumberOfServings());
            log.debug("RecipeForm.numberOfServings: is unchanged");
        }
        
        if(!fieldsToEscape.contains("preparationTimeDuration") && form.getPreparationTimeDuration() != null && form.getPreparationTimeDuration().compareTo(actualEntity.getPreparationTimeDuration()) != 0) {
            expectedEntity.setPreparationTimeDuration(form.getPreparationTimeDuration());
            changeSW = true;
            log.debug("RecipeForm.preparationTimeDuration: {} is different as RecipeEntity.preparationTimeDuration: {}", form.getPreparationTimeDuration(), actualEntity.getPreparationTimeDuration());
        } else {
            expectedEntity.setPreparationTimeDuration(actualEntity.getPreparationTimeDuration());
            log.debug("RecipeForm.preparationTimeDuration: is unchanged");
        }

        if(!fieldsToEscape.contains("preparationTimeUnitId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getPreparationTimeUnitId()))
                && form.getPreparationTimeUnitId().compareTo(actualEntity.getPreparationTimeUnitId()) != 0) {
            expectedEntity.setPreparationTimeUnitId(form.getPreparationTimeUnitId());
            changeSW = true;
            log.debug("RecipeForm.preparationTimeUnitId: {} is different as RecipeEntity.preparationTimeUnitId: {}", form.getPreparationTimeUnitId(), actualEntity.getPreparationTimeUnitId());
        } else {
            expectedEntity.setPreparationTimeUnitId(actualEntity.getPreparationTimeUnitId());
            log.debug("RecipeForm.preparationTimeUnitId: is unchanged");
        }

        if(!fieldsToEscape.contains("cookingTimeDuration") && form.getCookingTimeDuration() != null && form.getCookingTimeDuration().compareTo(actualEntity.getCookingTimeDuration()) != 0) {
            expectedEntity.setCookingTimeDuration(form.getCookingTimeDuration());
            changeSW = true;
            log.debug("RecipeForm.cookingTimeDuration: {} is different as RecipeEntity.cookingTimeDuration: {}", form.getCookingTimeDuration(), actualEntity.getCookingTimeDuration());
        } else {
            expectedEntity.setCookingTimeDuration(actualEntity.getCookingTimeDuration());
            log.debug("RecipeForm.cookingTimeDuration: is unchanged");
        }

        if(!fieldsToEscape.contains("cookingTimeUnitId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCookingTimeUnitId()))
                && form.getCookingTimeUnitId().compareTo(actualEntity.getCookingTimeUnitId()) != 0) {
            expectedEntity.setCookingTimeUnitId(form.getCookingTimeUnitId());
            changeSW = true;
            log.debug("RecipeForm.cookingTimeUnitId: {} is different as RecipeEntity.cookingTimeUnitId: {}", form.getCookingTimeUnitId(), actualEntity.getCookingTimeUnitId());
        } else {
            expectedEntity.setCookingTimeUnitId(actualEntity.getCookingTimeUnitId());
            log.debug("RecipeForm.cookingTimeUnitId: is unchanged");
        }

        if(!fieldsToEscape.contains("portionSizeAmount") && form.getPortionSizeAmount() != null && form.getPortionSizeAmount().compareTo(actualEntity.getPortionSizeAmount()) != 0) {
            expectedEntity.setPortionSizeAmount(form.getPortionSizeAmount());
            changeSW = true;
            log.debug("RecipeForm.portionSizeAmount: {} is different as RecipeEntity.portionSizeAmount: {}", form.getPortionSizeAmount(), actualEntity.getPortionSizeAmount());
        } else {
            expectedEntity.setPortionSizeAmount(actualEntity.getPortionSizeAmount());
            log.debug("RecipeForm.portionSizeAmount: is unchanged");
        }

        if(!fieldsToEscape.contains("portionSizeUnitId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getPortionSizeUnitId()))
                && form.getPortionSizeUnitId().compareTo(actualEntity.getPortionSizeUnitId()) != 0) {
            expectedEntity.setPortionSizeUnitId(form.getPortionSizeUnitId());
            changeSW = true;
            log.debug("RecipeForm.portionSizeUnitId: {} is different as RecipeEntity.portionSizeUnitId: {}", form.getPortionSizeUnitId(), actualEntity.getPortionSizeUnitId());
        } else {
            expectedEntity.setPortionSizeUnitId(actualEntity.getPortionSizeUnitId());
            log.debug("RecipeForm.portionSizeUnitId: is unchanged");
        }

        if(!fieldsToEscape.contains("itemId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getItemId()))
                && form.getItemId().compareTo(actualEntity.getItemId()) != 0) {
            expectedEntity.setItemId(form.getItemId());
            changeSW = true;
            log.debug("RecipeForm.itemId: {} is different as RecipeEntity.itemId: {}", form.getItemId(), actualEntity.getItemId());
        } else {
            expectedEntity.setItemId(actualEntity.getItemId());
            log.debug("RecipeForm.itemId: is unchanged");
        }

        if(!fieldsToEscape.contains("cuisineId") && form.getCuisineId() != null) {
            Long cuisineId = Long.parseLong(form.getCuisineId());
            Optional<CuisineEntity> optCuisineEntity = cuisineRepository.findById(cuisineId);
            if(actualEntity.getCuisine().compareTo(optCuisineEntity.get()) != 0) {
                expectedEntity.setCuisine(optCuisineEntity.get());
                changeSW = true;
                log.debug("RecipeForm.cuisineId: {} is different as RecipeEntity.cuisineId: {}", form.getCuisineId(), actualEntity.getCuisine().getId());
            } else {
                expectedEntity.setCuisine(actualEntity.getCuisine());
                log.debug("RecipeForm.cuisineId: is unchanged");
            }
        } else {
            expectedEntity.setCuisine(actualEntity.getCuisine());
            log.debug("RecipeForm.cuisineId: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
