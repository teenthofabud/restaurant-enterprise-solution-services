package com.teenthofabud.restaurant.solution.cookbook.ingredient.converter;

import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.repository.RecipeRepository;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientEntity;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class IngredientForm2EntityConverter implements Converter<IngredientForm, IngredientEntity> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.cookbook.ingredient.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private RecipeRepository recipeRepository;

    @Autowired
    public void setRecipeRepository(RecipeRepository recipeRepository) {
        this.recipeRepository = recipeRepository;
    }

    @Override
    public IngredientEntity convert(IngredientForm form) {
        IngredientEntity entity = new IngredientEntity();
        if(!fieldsToEscape.contains("name")) {
            entity.setName(form.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            entity.setDescription(form.getDescription());
        }
        if(!fieldsToEscape.contains("recipeId")) {
            Long recipeId = Long.parseLong(form.getRecipeId());
            Optional<RecipeEntity> optRecipeEntity = recipeRepository.findById(recipeId);
            entity.setRecipe(optRecipeEntity.get());
        }
        if(!fieldsToEscape.contains("productId")) {
            entity.setProductId(form.getProductId());
        }
        if(!fieldsToEscape.contains("quantityAmount")) {
            entity.setQuantityAmount(form.getQuantityAmount());
        }
        if(!fieldsToEscape.contains("quantityUnitId")) {
            entity.setQuantityUnitId(form.getQuantityUnitId());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
