package com.teenthofabud.restaurant.solution.session.association.data;

import lombok.Getter;

@Getter
public enum AssociationMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_ASSOCIATION_ENTITY_ID("Searching for ASSOCIATION Entity with id: {}"),
    MSG_TEMPLATE_NO_ASSOCIATION_ENTITY_ID_AVAILABLE("No ASSOCIATION Entity available with id: {}"),
    MSG_TEMPLATE_FOUND_ASSOCIATION_ENTITY_ID("Found ASSOCIATION Entity with id: {}"),
    MSG_TEMPLATE_ASSOCIATION_ID_VALID("ASSOCIATION  id: {} is semantically valid"),
    MSG_TEMPLATE_ASSOCIATION_ID_INVALID("ASSOCIATION  id: {} is invalid"),
    MSG_TEMPLATE_ASSOCIATION_ID_EMPTY("ASSOCIATION  id is empty"),
    MSG_TEMPLATE_ASSOCIATION_CASCADE_LEVEL_EMPTY("ASSOCIATION  cascadeLevel is empty"),
    MSG_TEMPLATE_ASSOCIATION_EXISTENCE_BY_TABLE_ID_AND_ACCOUNT_ID("Checking Association existence by tableId: {} and accountId: {}"),
    MSG_TEMPLATE_ASSOCIATION_EXISTS_BY_TABLE_ID_AND_ACCOUNT_ID("Association exists between tableId: {} and accountId: {}"),
    MSG_TEMPLATE_ASSOCIATION_NON_EXISTENCE_BY_TABLE_ID_AND_ACCOUNT_ID("No Association exists between tableId: {} and accountId: {}");

    private String value;

    private AssociationMessageTemplate(String value) {
        this.value = value;
    }


}
