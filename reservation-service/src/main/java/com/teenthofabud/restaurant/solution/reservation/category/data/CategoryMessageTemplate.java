package com.teenthofabud.restaurant.solution.reservation.category.data;

import lombok.Getter;

@Getter
public enum CategoryMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_CATEGORY_ENTITY_ID("Searching for category Entity with id: {}"),
    MSG_TEMPLATE_NO_CATEGORY_ENTITY_ID_AVAILABLE("No category Entity available with id: {}"),
    MSG_TEMPLATE_FOUND_CATEGORY_ENTITY_ID("Found category Entity with id: {}"),
    MSG_TEMPLATE_CATEGORY_ID_VALID("category  id: {} is semantically valid"),
    MSG_TEMPLATE_CATEGORY_ID_INVALID("category  id: {} is invalid"),
    MSG_TEMPLATE_CATEGORY_ID_EMPTY("category  id is empty"),
    MSG_TEMPLATE_CATEGORY_CASCADE_LEVEL_EMPTY("category  cascadeLevel is empty"),
    MSG_TEMPLATE_CATEGORY_EXISTENCE_BY_NAME ("Checking existence of category Entity with name: {}"),
    MSG_TEMPLATE_CATEGORY_EXISTS_BY_NAME ("category Entity already exists with name: {}"),
    MSG_TEMPLATE_CATEGORY_NON_EXISTENCE_BY_NAME ("No category Entity exists with name: {}");

    private String value;

    private CategoryMessageTemplate(String value) {
        this.value = value;
    }


}
