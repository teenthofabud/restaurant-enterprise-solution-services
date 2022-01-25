package com.teenthofabud.restaurant.solution.establishmentarea.table.data;

import lombok.Getter;

@Getter
public enum TableMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_TABLE_ENTITY_ID("Searching for TableEntity with id: {}"),
    MSG_TEMPLATE_NO_TABLE_ENTITY_ID_AVAILABLE("No TableEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_TABLE_ENTITY_ID("Found TableEntity with id: {}"),
    MSG_TEMPLATE_TABLE_ID_VALID("Table id: {} is semantically valid"),
    MSG_TEMPLATE_TABLE_ID_INVALID("Table id: {} is invalid"),
    MSG_TEMPLATE_TABLE_ID_EMPTY("Table id is empty"),
    MSG_TEMPLATE_TABLE_CASCADE_LEVEL_EMPTY("Table cascadeLevel is empty"),
    MSG_TEMPLATE_TABLE_EXISTENCE_BY_NAME ("Checking existence of TableEntity with name: {}"),
    MSG_TEMPLATE_TABLE_EXISTS_BY_NAME ("TableEntity already exists with name: {}"),
    MSG_TEMPLATE_TABLE_NON_EXISTENCE_BY_NAME ("No TableEntity exists with name: {}"),
    MSG_TEMPLATE_TABLE_EXISTENCE_BY_FLOOR_ID ("Checking existence of TableEntity with floor id: {}"),
    MSG_TEMPLATE_TABLE_EXISTS_BY_FLOOR_ID ("TableEntity already exists with floor id: {}"),
    MSG_TEMPLATE_TABLE_NON_EXISTENCE_BY_FLOOR_ID ("No TableEntity exists with floor id: {}"),

    MSG_TEMPLATE_TABLE_EXISTENCE_BY_NAME_AND_FLOOR_ID ("Checking existence of TableEntity with name: {} and floor id: {}"),
    MSG_TEMPLATE_TABLE_EXISTS_BY_NAME_AND_FLOOR_ID ("TableEntity already exists with name: {} and floor id: {}"),
    MSG_TEMPLATE_TABLE_NON_EXISTENCE_BY_NAME_AND_FLOOR_ID ("No TableEntity exists with name: {} and floor id: {}");

    private String value;

    private TableMessageTemplate(String value) {
        this.value = value;
    }


}
