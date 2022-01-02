package com.teenthofabud.restaurant.solution.inventory.product.data;

import lombok.Getter;

@Getter
public enum ProductMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_PRODUCT_ENTITY_ID("Searching for product Entity with id: {}"),
    MSG_TEMPLATE_NO_PRODUCT_ENTITY_ID_AVAILABLE("No product Entity available with id: {}"),
    MSG_TEMPLATE_FOUND_PRODUCT_ENTITY_ID("Found product Entity with id: {}"),
    MSG_TEMPLATE_PRODUCT_ID_VALID("product  id: {} is semantically valid"),
    MSG_TEMPLATE_PRODUCT_ID_INVALID("product  id: {} is invalid"),
    MSG_TEMPLATE_PRODUCT_ID_EMPTY("product  id is empty"),
    MSG_TEMPLATE_PRODUCT_CASCADE_LEVEL_EMPTY("product  cascadeLevel is empty"),
    MSG_TEMPLATE_PRODUCT_EXISTENCE_BY_NAME_AND_CATEGORY_ID ("Checking existence of product Entity with name: {} and categoryId: {}"),
    MSG_TEMPLATE_PRODUCT_EXISTS_BY_NAME_AND_CATEGORY_ID ("product Entity already exists with name: {} and categoryId: {}"),
    MSG_TEMPLATE_PRODUCT_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID ("No product Entity exists with name: {} and categoryId: {}");

    private String value;

    private ProductMessageTemplate(String value) {
        this.value = value;
    }


}
