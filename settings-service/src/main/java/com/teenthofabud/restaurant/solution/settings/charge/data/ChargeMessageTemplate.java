package com.teenthofabud.restaurant.solution.settings.charge.data;

import lombok.Getter;

@Getter
public enum ChargeMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_CHARGE_ENTITY_ID("Searching for charge Entity with id: {}"),
    MSG_TEMPLATE_NO_CHARGE_ENTITY_ID_AVAILABLE("No charge Entity available with id: {}"),
    MSG_TEMPLATE_FOUND_CHARGE_ENTITY_ID("Found charge Entity with id: {}"),
    MSG_TEMPLATE_CHARGE_ID_VALID("charge  id: {} is semantically valid"),
    MSG_TEMPLATE_CHARGE_ID_INVALID("charge  id: {} is invalid"),
    MSG_TEMPLATE_CHARGE_ID_EMPTY("charge  id is empty"),
    MSG_TEMPLATE_CHARGE_CASCADE_LEVEL_EMPTY("charge  cascadeLevel is empty"),
    MSG_TEMPLATE_CHARGE_EXISTENCE_BY_NAME ("Checking existence of charge Entity with name: {}"),
    MSG_TEMPLATE_CHARGE_EXISTS_BY_NAME ("charge Entity already exists with name: {}"),
    MSG_TEMPLATE_CHARGE_NON_EXISTENCE_BY_NAME ("No charge Entity exists with name: {}");

    private String value;

    private ChargeMessageTemplate(String value) {
        this.value = value;
    }


}
