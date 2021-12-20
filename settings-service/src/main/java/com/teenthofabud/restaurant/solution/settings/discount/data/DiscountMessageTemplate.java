package com.teenthofabud.restaurant.solution.settings.discount.data;

import lombok.Getter;

@Getter
public enum DiscountMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_DISCOUNT_ENTITY_ID("Searching for DISCOUNT Entity with id: {}"),
    MSG_TEMPLATE_NO_DISCOUNT_ENTITY_ID_AVAILABLE("No DISCOUNT Entity available with id: {}"),
    MSG_TEMPLATE_FOUND_DISCOUNT_ENTITY_ID("Found DISCOUNT Entity with id: {}"),
    MSG_TEMPLATE_DISCOUNT_ID_VALID("DISCOUNT  id: {} is semantically valid"),
    MSG_TEMPLATE_DISCOUNT_ID_INVALID("DISCOUNT  id: {} is invalid"),
    MSG_TEMPLATE_DISCOUNT_ID_EMPTY("DISCOUNT  id is empty"),
    MSG_TEMPLATE_DISCOUNT_CASCADE_LEVEL_EMPTY("DISCOUNT  cascadeLevel is empty"),
    MSG_TEMPLATE_DISCOUNT_EXISTENCE_BY_NAME ("Checking existence of DISCOUNT Entity with name: {}"),
    MSG_TEMPLATE_DISCOUNT_EXISTS_BY_NAME ("DISCOUNT Entity already exists with name: {}"),
    MSG_TEMPLATE_DISCOUNT_NON_EXISTENCE_BY_NAME ("No DISCOUNT Entity exists with name: {}");

    private String value;

    private DiscountMessageTemplate(String value) {
        this.value = value;
    }


}
