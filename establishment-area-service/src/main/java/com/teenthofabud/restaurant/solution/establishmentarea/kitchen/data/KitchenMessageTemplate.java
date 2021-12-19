package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data;

import lombok.Getter;

@Getter
public enum KitchenMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_KITCHEN_ENTITY_ID("Searching for KitchenEntity with id: {}"),
    MSG_TEMPLATE_NO_KITCHEN_ENTITY_ID_AVAILABLE("No KitchenEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_KITCHEN_ENTITY_ID("Found KitchenEntity with id: {}"),
    MSG_TEMPLATE_KITCHEN_ID_VALID("Kitchen id: {} is semantically valid"),
    MSG_TEMPLATE_KITCHEN_ID_INVALID("Kitchen id: {} is invalid"),
    MSG_TEMPLATE_KITCHEN_ID_EMPTY("Kitchen id is empty"),
    MSG_TEMPLATE_KITCHEN_CASCADE_LEVEL_EMPTY("Kitchen cascadeLevel is empty");

    private String value;

    private KitchenMessageTemplate(String value) {
        this.value = value;
    }


}