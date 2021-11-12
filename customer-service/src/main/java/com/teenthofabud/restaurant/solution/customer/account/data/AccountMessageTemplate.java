package com.teenthofabud.restaurant.solution.customer.account.data;

import aj.org.objectweb.asm.TypeReference;
import lombok.Getter;

@Getter
public enum AccountMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_ACCOUNT_ENTITY_ID("Searching for AccountEntity with id: {}"),
    MSG_TEMPLATE_NO_ACCOUNT_ENTITY_ID_AVAILABLE("No AccountEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_ACCOUNT_ENTITY_ID("Found AccountEntity with id: {}"),
    MSG_TEMPLATE_ACCOUNT_ID_VALID("account id: {} is semantically valid"),
    MSG_TEMPLATE_ACCOUNT_ID_INVALID("account id: {} is invalid"),
    MSG_TEMPLATE_ACCOUNT_DOB_INVALID("account dateOfBirth: {} is invalid"),
    MSG_TEMPLATE_ACCOUNT_ID_EMPTY("account id is empty"),
    MSG_TEMPLATE_ACCOUNT_CASCADE_LEVEL_EMPTY("account cascadeLevel is empty"),
    MSG_TEMPLATE_ACCOUNT_EXISTENCE_BY_PHONE_NUMBER ("Checking existence of AccountEntity with phone number: {}"),
    MSG_TEMPLATE_ACCOUNT_EXISTS_BY_PHONE_NUMBER ("AccountEntity already exists with phone number: {}"),
    MSG_TEMPLATE_ACCOUNT_NON_EXISTENCE_BY_PHONE_NUMBER ("No AccountEntity exists with phone number: {}");

    private String value;

    private AccountMessageTemplate(String value) {
        this.value = value;
    }


}
