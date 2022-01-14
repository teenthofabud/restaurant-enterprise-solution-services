package com.teenthofabud.restaurant.solution.cookbook.recipe.data;

import lombok.Getter;
import org.springframework.asm.TypeReference;

@Getter
public enum RecipeMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_RECIPE_ENTITY_ID("Searching for Recipe Entity with id: {}"),
    MSG_TEMPLATE_NO_RECIPE_ENTITY_ID_AVAILABLE("No Recipe Entity available with id: {}"),
    MSG_TEMPLATE_FOUND_RECIPE_ENTITY_ID("Found Recipe Entity with id: {}"),
    MSG_TEMPLATE_RECIPE_ID_VALID("recipe id: {} is semantically valid"),
    MSG_TEMPLATE_RECIPE_ID_INVALID("recipe id: {} is invalid"),
    MSG_TEMPLATE_RECIPE_CUISINE_ID_INVALID("recipe cityId: {} is invalid"),
    MSG_TEMPLATE_RECIPE_ITEM_ID_INVALID("recipe stateId: {} is invalid"),
    MSG_TEMPLATE_RECIPE_ID_EMPTY("recipe id is empty"),
    MSG_TEMPLATE_RECIPE_CASCADE_LEVEL_EMPTY("recipe cascadeLevel is empty"),
    MSG_TEMPLATE_RECIPE_EXISTENCE_BY_NAME ("Checking existence of Recipe Entity with name: {}"),
    MSG_TEMPLATE_RECIPE_EXISTS_BY_NAME ("Recipe Entity already exists with name: {}"),
    MSG_TEMPLATE_RECIPE_NON_EXISTENCE_BY_NAME ("No Recipe Entity exists with name: {}"),
    MSG_TEMPLATE_RECIPE_EXISTENCE_BY_ITEM_ID ("Checking existence of Recipe Entity with item id: {}"),
    MSG_TEMPLATE_RECIPE_EXISTS_BY_ITEM_ID ("Recipe Entity already exists with item id: {}"),
    MSG_TEMPLATE_RECIPE_NON_EXISTENCE_BY_ITEM_ID ("No Recipe Entity exists with item id: {}"),

    MSG_TEMPLATE_RECIPE_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID ("Checking existence of Recipe Entity with name: {} and cuisine id: {} and item id: {}"),
    MSG_TEMPLATE_RECIPE_EXISTS_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID ("Recipe Entity already exists with name: {} and cuisine id: {} and item id: {}"),
    MSG_TEMPLATE_RECIPE_NON_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID ("No Recipe Entity exists with name: {} and cuisine id: {} and item id: {}"),

    MSG_TEMPLATE_RECIPE_PREPARATION_TIME_DURATION_INVALID("recipe preparationTimeDuration: {} is invalid"),
    MSG_TEMPLATE_RECIPE_PREPARATION_TIME_UNIT_ID_INVALID("recipe preparationTimeUnitId: {} is invalid"),
    MSG_TEMPLATE_RECIPE_COOKING_TIME_DURATION_INVALID("recipe cookingTimeDuration: {} is invalid"),
    MSG_TEMPLATE_RECIPE_COOKING_TIME_UNIT_ID_INVALID("recipe cookingTimeUnitId: {} is invalid"),
    MSG_TEMPLATE_RECIPE_PORTION_SIZE_AMOUNT_INVALID("recipe portionSizeAmount: {} is invalid"),
    MSG_TEMPLATE_RECIPE_PORTION_SIZE_TIME_UNIT_ID_INVALID("recipe portionSizeUnitId: {} is invalid");
    private String value;

    private RecipeMessageTemplate(String value) {
        this.value = value;
    }


}
