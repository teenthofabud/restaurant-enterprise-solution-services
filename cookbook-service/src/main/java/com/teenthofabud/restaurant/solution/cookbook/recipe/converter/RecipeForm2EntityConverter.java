package com.teenthofabud.restaurant.solution.cookbook.recipe.converter;

import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.repository.CuisineRepository;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class RecipeForm2EntityConverter implements Converter<RecipeForm, RecipeEntity> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.cookbook.recipe.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private CuisineRepository cuisineRepository;

    @Autowired
    public void setCuisineRepository(CuisineRepository cuisineRepository) {
        this.cuisineRepository = cuisineRepository;
    }

    @Override
    public RecipeEntity convert(RecipeForm form) {
        RecipeEntity entity = new RecipeEntity();
        if(!fieldsToEscape.contains("name")) {
            entity.setName(form.getName());
        }
        if(!fieldsToEscape.contains("instructions")) {
            entity.setInstructions(form.getInstructions());
        }
        if(!fieldsToEscape.contains("description")) {
            entity.setDescription(form.getDescription());
        }
        if(!fieldsToEscape.contains("cuisineId")) {
            Long cuisineId = Long.parseLong(form.getCuisineId());
            Optional<CuisineEntity> optCuisineEntity = cuisineRepository.findById(cuisineId);
            entity.setCuisine(optCuisineEntity.get());
        }
        if(!fieldsToEscape.contains("itemId")) {
            entity.setItemId(form.getItemId());
        }
        if(!fieldsToEscape.contains("cookingMethod")) {
            entity.setCookingMethod(form.getCookingMethod());
        }
        if(!fieldsToEscape.contains("numberOfServings")) {
            entity.setNumberOfServings(form.getNumberOfServings());
        }
        if(!fieldsToEscape.contains("preparationTimeDuration")) {
            entity.setPreparationTimeDuration(form.getPreparationTimeDuration());
        }
        if(!fieldsToEscape.contains("preparationTimeUnitId")) {
            entity.setPreparationTimeUnitId(form.getPreparationTimeUnitId());
        }
        if(!fieldsToEscape.contains("cookingTimeDuration")) {
            entity.setCookingTimeDuration(form.getCookingTimeDuration());
        }
        if(!fieldsToEscape.contains("cookingTimeUnitId")) {
            entity.setCookingTimeUnitId(form.getCookingTimeUnitId());
        }
        if(!fieldsToEscape.contains("portionSizeAmount")) {
            entity.setPortionSizeAmount(form.getPortionSizeAmount());
        }
        if(!fieldsToEscape.contains("portionSizeUnitId")) {
            entity.setPortionSizeUnitId(form.getPortionSizeUnitId());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
