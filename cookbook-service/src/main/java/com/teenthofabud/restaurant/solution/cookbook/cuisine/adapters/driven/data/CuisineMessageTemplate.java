package com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data;

import lombok.Getter;

@Getter
public enum CuisineMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_CUISINE_ENTITY_ID("Searching for Cuisine Entity with id: {}"),
    MSG_TEMPLATE_NO_CUISINE_ENTITY_ID_AVAILABLE("No Cuisine Entity available with id: {}"),
    MSG_TEMPLATE_FOUND_CUISINE_ENTITY_ID("Found Cuisine Entity with id: {}"),
    MSG_TEMPLATE_CUISINE_ID_VALID("cuisine id: {} is semantically valid"),
    MSG_TEMPLATE_CUISINE_ID_INVALID("cuisine id: {} is invalid"),
    MSG_TEMPLATE_CUISINE_ID_EMPTY("cuisine id is empty"),
    MSG_TEMPLATE_CUISINE_CASCADE_LEVEL_EMPTY("cuisine cascadeLevel is empty"),
    MSG_TEMPLATE_CUISINE_EXISTENCE_BY_NAME ("Checking existence of Cuisine Entity with name: {}"),
    MSG_TEMPLATE_CUISINE_EXISTS_BY_NAME ("Cuisine Entity already exists with name: {}"),
    MSG_TEMPLATE_CUISINE_NON_EXISTENCE_BY_NAME ("No Cuisine Entity exists with name: {}");

    private String value;

    private CuisineMessageTemplate(String value) {
        this.value = value;
    }


}
