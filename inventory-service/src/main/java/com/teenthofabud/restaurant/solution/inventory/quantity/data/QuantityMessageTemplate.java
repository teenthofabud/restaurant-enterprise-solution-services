package com.teenthofabud.restaurant.solution.inventory.quantity.data;

import lombok.Getter;

@Getter
public enum QuantityMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_QUANTITY_ENTITY_ID("Searching for quantity Entity with id: {}"),
    MSG_TEMPLATE_NO_QUANTITY_ENTITY_ID_AVAILABLE("No quantity Entity available with id: {}"),
    MSG_TEMPLATE_FOUND_QUANTITY_ENTITY_ID("Found quantity Entity with id: {}"),
    MSG_TEMPLATE_QUANTITY_ID_VALID("quantity  id: {} is semantically valid"),
    MSG_TEMPLATE_QUANTITY_ID_INVALID("quantity  id: {} is invalid"),
    MSG_TEMPLATE_QUANTITY_ID_EMPTY("quantity  id is empty"),
    MSG_TEMPLATE_QUANTITY_CASCADE_LEVEL_EMPTY("quantity  cascadeLevel is empty"),
    MSG_TEMPLATE_QUANTITY_EXISTENCE_BY_PRODUCT_ID_AND_WEIGHT_ID ("Checking existence of quantity Entity with productId: {} and weightId: {}"),
    MSG_TEMPLATE_QUANTITY_EXISTS_BY_PRODUCT_ID_AND_WEIGHT_ID ("quantity Entity already exists with productId: {} and weightId: {}"),
    MSG_TEMPLATE_QUANTITY_NON_EXISTENCE_BY_PRODUCT_ID_AND_WEIGHT_ID ("No quantity Entity exists with productId: {} and weightId: {}");

    private String value;

    private QuantityMessageTemplate(String value) {
        this.value = value;
    }


}
