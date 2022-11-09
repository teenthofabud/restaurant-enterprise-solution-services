package com.teenthofabud.restaurant.solution.engagement.tableallocation.data;

import lombok.Getter;

@Getter
public enum TableAllocationMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_TABLE_ALLOCATION_ENTITY_ID("Searching for TABLE_ALLOCATION Document with id: {}"),
    MSG_TEMPLATE_NO_TABLE_ALLOCATION_ENTITY_ID_AVAILABLE("No TABLE_ALLOCATION Document available with id: {}"),
    MSG_TEMPLATE_FOUND_TABLE_ALLOCATION_ENTITY_ID("Found TABLE_ALLOCATION Document with id: {}"),
    MSG_TEMPLATE_TABLE_ALLOCATION_ID_VALID("TABLE_ALLOCATION  id: {} is semantically valid"),
    MSG_TEMPLATE_TABLE_ALLOCATION_ID_INVALID("TABLE_ALLOCATION  id: {} is invalid"),
    MSG_TEMPLATE_TABLE_ALLOCATION_ID_EMPTY("TABLE_ALLOCATION  id is empty"),
    MSG_TEMPLATE_TABLE_ALLOCATION_CASCADE_LEVEL_EMPTY("TABLE_ALLOCATION  cascadeLevel is empty"),
    MSG_TEMPLATE_TABLE_ALLOCATION_EXISTENCE_BY_CHECK_IN_ID_AND_TABLE_ID_AND_ACTIVE("Checking TableAllocation existence by checkInId: {}, tableId: {} and active: {}"),
    MSG_TEMPLATE_TABLE_ALLOCATION_EXISTS_BY_CHECK_IN_ID_AND_TABLE_ID_AND_ACTIVE("TableAllocation exists between checkInId: {}, tableId: {} and active: {}"),
    MSG_TEMPLATE_TABLE_ALLOCATION_NON_EXISTENCE_BY_CHECK_IN_ID_AND_TABLE_ID_AND_ACTIVE("No TableAllocation exists between checkInId: {}, tableId: {} and active: {}");

    private String value;

    private TableAllocationMessageTemplate(String value) {
        this.value = value;
    }


}
