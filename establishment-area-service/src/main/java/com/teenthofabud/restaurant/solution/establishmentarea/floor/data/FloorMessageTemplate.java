package com.teenthofabud.restaurant.solution.establishmentarea.floor.data;

import lombok.Getter;

@Getter
public enum FloorMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_FLOOR_ENTITY_ID("Searching for FloorEntity with id: {}"),
    MSG_TEMPLATE_NO_FLOOR_ENTITY_ID_AVAILABLE("No FloorEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_FLOOR_ENTITY_ID("Found FloorEntity with id: {}"),
    MSG_TEMPLATE_FLOOR_ID_VALID("Floor id: {} is semantically valid"),
    MSG_TEMPLATE_FLOOR_ID_INVALID("Floor id: {} is invalid"),
    MSG_TEMPLATE_FLOOR_ID_EMPTY("Floor id is empty"),
    MSG_TEMPLATE_FLOOR_CASCADE_LEVEL_EMPTY("Floor cascadeLevel is empty"),

    MSG_TEMPLATE_FLOOR_EXISTENCE_BY_NAME ("Checking existence of FloorEntity with name: {}"),
    MSG_TEMPLATE_FLOOR_EXISTS_BY_NAME ("FloorEntity already exists with name: {}"),
    MSG_TEMPLATE_FLOOR_NON_EXISTENCE_BY_NAME ("No FloorEntity exists with name: {}");

    private String value;

    private FloorMessageTemplate(String value) {
        this.value = value;
    }


}
