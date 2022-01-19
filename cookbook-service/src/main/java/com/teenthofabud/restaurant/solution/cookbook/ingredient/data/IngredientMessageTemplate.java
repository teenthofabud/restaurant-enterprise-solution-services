package com.teenthofabud.restaurant.solution.cookbook.ingredient.data;

import lombok.Getter;

@Getter
public enum IngredientMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_INGREDIENT_ENTITY_ID("Searching for Ingredient Entity with id: {}"),
    MSG_TEMPLATE_NO_INGREDIENT_ENTITY_ID_AVAILABLE("No Ingredient Entity available with id: {}"),
    MSG_TEMPLATE_FOUND_INGREDIENT_ENTITY_ID("Found Ingredient Entity with id: {}"),
    MSG_TEMPLATE_INGREDIENT_ID_VALID("ingredient id: {} is semantically valid"),
    MSG_TEMPLATE_INGREDIENT_ID_INVALID("ingredient id: {} is invalid"),
    MSG_TEMPLATE_INGREDIENT_INGREDIENT_ID_INVALID("ingredient ingredient Id: {} is invalid"),
    MSG_TEMPLATE_INGREDIENT_PRODUCT_ID_INVALID("ingredient product Id: {} is invalid"),
    MSG_TEMPLATE_INGREDIENT_ID_EMPTY("ingredient id is empty"),
    MSG_TEMPLATE_INGREDIENT_CASCADE_LEVEL_EMPTY("ingredient cascadeLevel is empty"),
    MSG_TEMPLATE_INGREDIENT_EXISTENCE_BY_NAME ("Checking existence of Ingredient Entity with name: {}"),
    MSG_TEMPLATE_INGREDIENT_EXISTS_BY_NAME ("Ingredient Entity already exists with name: {}"),
    MSG_TEMPLATE_INGREDIENT_NON_EXISTENCE_BY_NAME ("No Ingredient Entity exists with name: {}"),
    MSG_TEMPLATE_INGREDIENT_EXISTENCE_BY_PRODUCT_ID ("Checking existence of Ingredient Entity with product id: {}"),
    MSG_TEMPLATE_INGREDIENT_EXISTS_BY_PRODUCT_ID ("Ingredient Entity already exists with product id: {}"),
    MSG_TEMPLATE_INGREDIENT_NON_EXISTENCE_BY_PRODUCT_ID ("No Ingredient Entity exists with product id: {}"),
    MSG_TEMPLATE_INGREDIENT_EXISTENCE_BY_NAME_AND_RECIPE_ID_AND_PRODUCT_ID ("Checking existence of Ingredient Entity with name: {} and recipe id: {} and product id: {}"),
    MSG_TEMPLATE_INGREDIENT_EXISTS_BY_NAME_AND_RECIPE_ID_AND_PRODUCT_ID ("Ingredient Entity already exists with name: {} and recipe id: {} and product id: {}"),
    MSG_TEMPLATE_INGREDIENT_NON_EXISTENCE_BY_NAME_AND_RECIPE_ID_AND_PRODUCT_ID ("No Ingredient Entity exists with name: {} and recipe id: {} and product id: {}"),
    MSG_TEMPLATE_INGREDIENT_QUANTITY_AMOUNT_INVALID("ingredient quantity Amount: {} is invalid"),
    MSG_TEMPLATE_INGREDIENT_QUANTITY_UNIT_ID_INVALID("ingredient quantity unit id: {} is invalid");
    private String value;

    private IngredientMessageTemplate(String value) {
        this.value = value;
    }


}
