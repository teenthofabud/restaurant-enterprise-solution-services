package com.teenthofabud.restaurant.solution.settings.paymentmethod.data;

import lombok.Getter;

@Getter
public enum PaymentMethodMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_PAYMENT_METHOD_ENTITY_ID("Searching for payment method Entity with id: {}"),
    MSG_TEMPLATE_NO_PAYMENT_METHOD_ENTITY_ID_AVAILABLE("No payment method Entity available with id: {}"),
    MSG_TEMPLATE_FOUND_PAYMENT_METHOD_ENTITY_ID("Found payment method Entity with id: {}"),
    MSG_TEMPLATE_PAYMENT_METHOD_ID_VALID("payment method  id: {} is semantically valid"),
    MSG_TEMPLATE_PAYMENT_METHOD_ID_INVALID("payment method  id: {} is invalid"),
    MSG_TEMPLATE_PAYMENT_METHOD_ID_EMPTY("payment method  id is empty"),
    MSG_TEMPLATE_PAYMENT_METHOD_CASCADE_LEVEL_EMPTY("payment method  cascadeLevel is empty"),
    MSG_TEMPLATE_PAYMENT_METHOD_EXISTENCE_BY_NAME ("Checking existence of payment method Entity with name: {}"),
    MSG_TEMPLATE_PAYMENT_METHOD_EXISTS_BY_NAME ("payment method Entity already exists with name: {}"),
    MSG_TEMPLATE_PAYMENT_METHOD_NON_EXISTENCE_BY_NAME ("No payment method Entity exists with name: {}");

    private String value;

    private PaymentMethodMessageTemplate(String value) {
        this.value = value;
    }


}
