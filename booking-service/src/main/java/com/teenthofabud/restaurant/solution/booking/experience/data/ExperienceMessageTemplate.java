package com.teenthofabud.restaurant.solution.booking.experience.data;

import lombok.Getter;

@Getter
public enum ExperienceMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_EXPERIENCE_ENTITY_ID("Searching for experience Entity with id: {}"),
    MSG_TEMPLATE_NO_EXPERIENCE_ENTITY_ID_AVAILABLE("No experience Entity available with id: {}"),
    MSG_TEMPLATE_FOUND_EXPERIENCE_ENTITY_ID("Found experience Entity with id: {}"),
    MSG_TEMPLATE_EXPERIENCE_ID_VALID("experience  id: {} is semantically valid"),
    MSG_TEMPLATE_EXPERIENCE_ID_INVALID("experience  id: {} is invalid"),
    MSG_TEMPLATE_EXPERIENCE_ID_EMPTY("experience  id is empty"),
    MSG_TEMPLATE_EXPERIENCE_CASCADE_LEVEL_EMPTY("experience  cascadeLevel is empty"),
    MSG_TEMPLATE_EXPERIENCE_EXISTENCE_BY_NAME ("Checking existence of experience Entity with name: {}"),
    MSG_TEMPLATE_EXPERIENCE_EXISTS_BY_NAME ("experience Entity already exists with name: {}"),
    MSG_TEMPLATE_EXPERIENCE_NON_EXISTENCE_BY_NAME ("No experience Entity exists with name: {}");

    private String value;

    private ExperienceMessageTemplate(String value) {
        this.value = value;
    }


}
