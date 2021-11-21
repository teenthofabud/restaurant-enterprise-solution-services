package com.teenthofabud.restaurant.solution.customer.address.data;

import aj.org.objectweb.asm.TypeReference;
import lombok.Getter;

@Getter
public enum AddressMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_ADDRESS_ENTITY_ID("Searching for AddressEntity with id: {}"),
    MSG_TEMPLATE_NO_ADDRESS_ENTITY_ID_AVAILABLE("No AddressEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_ADDRESS_ENTITY_ID("Found AddressEntity with id: {}"),
    MSG_TEMPLATE_ADDRESS_ID_VALID("address id: {} is semantically valid"),
    MSG_TEMPLATE_ADDRESS_ID_INVALID("address id: {} is invalid"),
    MSG_TEMPLATE_ADDRESS_CITY_ID_INVALID("address cityId: {} is invalid"),
    MSG_TEMPLATE_ADDRESS_STATE_ID_INVALID("address stateId: {} is invalid"),
    MSG_TEMPLATE_ADDRESS_COUNTRY_ID_INVALID("address countryId: {} is invalid"),
    MSG_TEMPLATE_ADDRESS_ID_EMPTY("address id is empty"),
    MSG_TEMPLATE_ADDRESS_CASCADE_LEVEL_EMPTY("address cascadeLevel is empty"),
    MSG_TEMPLATE_ADDRESS_EXISTENCE_BY_NAME ("Checking existence of AddressEntity with name: {}"),
    MSG_TEMPLATE_ADDRESS_EXISTS_BY_NAME ("AddressEntity already exists with name: {}"),
    MSG_TEMPLATE_ADDRESS_NON_EXISTENCE_BY_NAME ("No AddressEntity exists with name: {}"),
    MSG_TEMPLATE_ADDRESS_EXISTENCE_BY_ACCOUNT_ID ("Checking existence of AddressEntity with account id: {}"),
    MSG_TEMPLATE_ADDRESS_EXISTS_BY_ACCOUNT_ID ("AddressEntity already exists with account id: {}"),
    MSG_TEMPLATE_ADDRESS_NON_EXISTENCE_BY_ACCOUNT_ID ("No AddressEntity exists with account id: {}"),

    MSG_TEMPLATE_ADDRESS_EXISTENCE_BY_NAME_AND_ACCOUNT_ID ("Checking existence of AddressEntity with name: {} and account id: {}"),
    MSG_TEMPLATE_ADDRESS_EXISTS_BY_NAME_AND_ACCOUNT_ID ("AddressEntity already exists with name: {} and account id: {}"),
    MSG_TEMPLATE_ADDRESS_NON_EXISTENCE_BY_NAME_AND_ACCOUNT_ID ("No AddressEntity exists with name: {} and account id: {}");

    private String value;

    private AddressMessageTemplate(String value) {
        this.value = value;
    }


}
