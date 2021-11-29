package com.teenthofabud.restaurant.solution.menu.item.data;

import lombok.Getter;

@Getter
public enum ItemMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_ITEM_ENTITY_ID("Searching for itemEntity with id: {}"),
    MSG_TEMPLATE_NO_ITEM_ENTITY_ID_AVAILABLE("No itemEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_ITEM_ENTITY_ID("Found itemEntity with id: {}"),
    MSG_TEMPLATE_ITEM_ID_VALID("item id: {} is semantically valid"),
    MSG_TEMPLATE_ITEM_ID_INVALID("item id: {} is invalid"),
    MSG_TEMPLATE_ITEM_ID_EMPTY("item id is empty"),
    MSG_TEMPLATE_ITEM_CASCADE_LEVEL_EMPTY("item cascadeLevel is empty"),
    MSG_TEMPLATE_ITEM_EXISTENCE_BY_NAME_AND_CATEGORY_ID ("Checking existence of itemEntity with name: {} and categoryId: {}"),
    MSG_TEMPLATE_ITEM_EXISTS_BY_NAME_AND_CATEGORY_ID ("itemEntity already exists with name: {} and categoryId: {}"),
    MSG_TEMPLATE_ITEM_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID ("No itemEntity exists with name: {} and categoryId: {}");

    private String value;

    private ItemMessageTemplate(String value) {
        this.value = value;
    }


}
