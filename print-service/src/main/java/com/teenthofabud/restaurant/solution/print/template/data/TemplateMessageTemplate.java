package com.teenthofabud.restaurant.solution.print.template.data;

import lombok.Getter;

@Getter
public enum TemplateMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_TEMPLATE_ENTITY_ID("Searching for templateEntity with id: {}"),
    MSG_TEMPLATE_NO_TEMPLATE_ENTITY_ID_AVAILABLE("No templateEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_TEMPLATE_ENTITY_ID("Found templateEntity with id: {}"),
    MSG_TEMPLATE_TEMPLATE_ID_VALID("template id: {} is semantically valid"),
    MSG_TEMPLATE_TEMPLATE_ID_INVALID("template id: {} is invalid"),
    MSG_TEMPLATE_TEMPLATE_DOB_INVALID("template dateOfBirth: {} is invalid"),
    MSG_TEMPLATE_TEMPLATE_ID_EMPTY("template id is empty"),
    MSG_TEMPLATE_TEMPLATE_CASCADE_LEVEL_EMPTY("template cascadeLevel is empty"),
    MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID ("Checking existence of templateEntity with name: {} and template type id: {}"),
    MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID ("templateEntity already exists with name: {} and template type id: {}"),
    MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID ("No templateEntity exists with name: {} and template type id: {}");

    private String value;

    private TemplateMessageTemplate(String value) {
        this.value = value;
    }


}
