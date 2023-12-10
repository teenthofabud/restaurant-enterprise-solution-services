package com.teenthofabud.restaurant.solution.settings.internationalization.data;

import lombok.Getter;

@Getter
public enum LanguageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_LANGUAGE_ENTITY_ID("Searching for delivery partner Entity with id: {}"),
    MSG_TEMPLATE_NO_LANGUAGE_ENTITY_ID_AVAILABLE("No language Entity available with id: {}"),
    MSG_TEMPLATE_FOUND_LANGUAGE_ENTITY_ID("Found language Entity with id: {}"),
    MSG_TEMPLATE_LANGUAGE_ID_VALID("language id: {} is semantically valid"),
    MSG_TEMPLATE_LANGUAGE_ID_INVALID("language id: {} is invalid"),
    MSG_TEMPLATE_LANGUAGE_ID_EMPTY("language id is empty"),
    MSG_TEMPLATE_LANGUAGE_CASCADE_LEVEL_EMPTY("language cascadeLevel is empty"),
    MSG_TEMPLATE_LANGUAGE_EXISTENCE_BY_NAME ("Checking existence of language Entity with name: {}"),
    MSG_TEMPLATE_LANGUAGE_EXISTS_BY_NAME ("language Entity already exists with name: {}"),
    MSG_TEMPLATE_LANGUAGE_NON_EXISTENCE_BY_NAME ("No language Entity exists with name: {}");

    private String value;

    private LanguageTemplate(String value) { this.value = value; }
}
