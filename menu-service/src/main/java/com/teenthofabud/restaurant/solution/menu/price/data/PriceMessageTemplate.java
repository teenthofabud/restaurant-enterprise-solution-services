package com.teenthofabud.restaurant.solution.menu.price.data;

import lombok.Getter;

@Getter
public enum PriceMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_PRICE_ENTITY_ID("Searching for priceEntity with id: {}"),
    MSG_TEMPLATE_NO_PRICE_ENTITY_ID_AVAILABLE("No priceEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_PRICE_ENTITY_ID("Found priceEntity with id: {}"),
    MSG_TEMPLATE_PRICE_ID_VALID("price id: {} is semantically valid"),
    MSG_TEMPLATE_PRICE_ID_INVALID("price id: {} is invalid"),
    MSG_TEMPLATE_PRICE_ID_EMPTY("price id is empty"),
    MSG_TEMPLATE_PRICE_CASCADE_LEVEL_EMPTY("price cascadeLevel is empty"),
    MSG_TEMPLATE_PRICE_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID ("Checking existence of priceEntity with itemId: {} and currencyId: {}"),
    MSG_TEMPLATE_PRICE_EXISTS_BY_ITEM_ID_AND_CURRENCY_ID ("priceEntity already exists with itemId: {} and currencyId: {}"),
    MSG_TEMPLATE_PRICE_NON_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID ("No priceEntity exists with itemId: {} and currencyId: {}");

    private String value;

    private PriceMessageTemplate(String value) {
        this.value = value;
    }


}
