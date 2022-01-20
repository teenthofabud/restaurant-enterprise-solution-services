package com.teenthofabud.restaurant.solution.cookbook.ingredient.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.repository.RecipeRepository;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientEntity;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class IngredientForm2EntityMapper implements DualChannelMapper<IngredientEntity, IngredientForm> {

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
    public Optional<IngredientEntity> compareAndMap(IngredientEntity actualEntity, IngredientForm form) {
        IngredientEntity expectedEntity = new IngredientEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying IngredientEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying IngredientEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying IngredientEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy

        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName()))
                && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("IngredientForm.name: {} is different as IngredientEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("IngredientForm.name: is unchanged");
        }

        if(!fieldsToEscape.contains("description") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription()))
                && form.getDescription().compareTo(actualEntity.getDescription()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("IngredientForm.description: {} is different as IngredientEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            log.debug("IngredientForm.description: is unchanged");
        }

        if(!fieldsToEscape.contains("quantityAmount") && form.getQuantityAmount() != null && form.getQuantityAmount().compareTo(actualEntity.getQuantityAmount()) != 0) {
            expectedEntity.setQuantityAmount(form.getQuantityAmount());
            changeSW = true;
            log.debug("IngredientForm.quantityAmount: {} is different as IngredientEntity.quantityAmount: {}", form.getQuantityAmount(), actualEntity.getQuantityAmount());
        } else {
            expectedEntity.setQuantityAmount(actualEntity.getQuantityAmount());
            log.debug("IngredientForm.quantityAmount: is unchanged");
        }

        if(!fieldsToEscape.contains("quantityUnitId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getQuantityUnitId()))
                && form.getQuantityUnitId().compareTo(actualEntity.getQuantityUnitId()) != 0) {
            expectedEntity.setQuantityUnitId(form.getQuantityUnitId());
            changeSW = true;
            log.debug("IngredientForm.quantityUnitId: {} is different as IngredientEntity.quantityUnitId: {}", form.getQuantityUnitId(), actualEntity.getQuantityUnitId());
        } else {
            expectedEntity.setQuantityUnitId(actualEntity.getQuantityUnitId());
            log.debug("IngredientForm.quantityUnitId: is unchanged");
        }

        if(!fieldsToEscape.contains("productId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getProductId()))
                && form.getProductId().compareTo(actualEntity.getProductId()) != 0) {
            expectedEntity.setProductId(form.getProductId());
            changeSW = true;
            log.debug("IngredientForm.productId: {} is different as IngredientEntity.productId: {}", form.getProductId(), actualEntity.getProductId());
        } else {
            expectedEntity.setProductId(actualEntity.getProductId());
            log.debug("IngredientForm.productId: is unchanged");
        }

        if(!fieldsToEscape.contains("recipeId") && form.getRecipeId() != null) {
            Long recipeId = Long.parseLong(form.getRecipeId());
            Optional<RecipeEntity> optRecipeEntity = recipeRepository.findById(recipeId);
            if(actualEntity.getRecipe().compareTo(optRecipeEntity.get()) != 0) {
                expectedEntity.setRecipe(optRecipeEntity.get());
                changeSW = true;
                log.debug("IngredientForm.recipeId: {} is different as IngredientEntity.recipeId: {}", form.getRecipeId(), actualEntity.getRecipe().getId());
            } else {
                expectedEntity.setRecipe(actualEntity.getRecipe());
                log.debug("IngredientForm.recipeId: is unchanged");
            }
        } else {
            expectedEntity.setRecipe(actualEntity.getRecipe());
            log.debug("IngredientForm.recipeId: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
