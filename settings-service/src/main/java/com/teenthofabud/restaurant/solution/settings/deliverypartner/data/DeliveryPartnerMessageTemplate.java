package com.teenthofabud.restaurant.solution.settings.deliverypartner.data;

import lombok.Getter;

@Getter
public enum DeliveryPartnerMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_DELIVERY_PARTNER_ENTITY_ID("Searching for delivery partner Entity with id: {}"),
    MSG_TEMPLATE_NO_DELIVERY_PARTNER_ENTITY_ID_AVAILABLE("No delivery partner Entity available with id: {}"),
    MSG_TEMPLATE_FOUND_DELIVERY_PARTNER_ENTITY_ID("Found delivery partner Entity with id: {}"),
    MSG_TEMPLATE_DELIVERY_PARTNER_ID_VALID("delivery partner  id: {} is semantically valid"),
    MSG_TEMPLATE_DELIVERY_PARTNER_ID_INVALID("delivery partner  id: {} is invalid"),
    MSG_TEMPLATE_DELIVERY_PARTNER_ID_EMPTY("delivery partner  id is empty"),
    MSG_TEMPLATE_DELIVERY_PARTNER_CASCADE_LEVEL_EMPTY("delivery partner  cascadeLevel is empty"),
    MSG_TEMPLATE_DELIVERY_PARTNER_EXISTENCE_BY_NAME ("Checking existence of delivery partner Entity with name: {}"),
    MSG_TEMPLATE_DELIVERY_PARTNER_EXISTS_BY_NAME ("delivery partner Entity already exists with name: {}"),
    MSG_TEMPLATE_DELIVERY_PARTNER_NON_EXISTENCE_BY_NAME ("No delivery partner Entity exists with name: {}");

    private String value;

    private DeliveryPartnerMessageTemplate(String value) {
        this.value = value;
    }


}
