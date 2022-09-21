package com.teenthofabud.restaurant.solution.engagement.engagement.data;

import lombok.Getter;

@Getter
public enum EngagementMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_ENGAGEMENT_ENTITY_ID("Searching for ENGAGEMENT Entity with id: {}"),
    MSG_TEMPLATE_NO_ENGAGEMENT_ENTITY_ID_AVAILABLE("No ENGAGEMENT Entity available with id: {}"),
    MSG_TEMPLATE_FOUND_ENGAGEMENT_ENTITY_ID("Found ENGAGEMENT Entity with id: {}"),
    MSG_TEMPLATE_ENGAGEMENT_ID_VALID("ENGAGEMENT  id: {} is semantically valid"),
    MSG_TEMPLATE_ENGAGEMENT_ID_INVALID("ENGAGEMENT  id: {} is invalid"),
    MSG_TEMPLATE_ENGAGEMENT_TIMESTAMP_INVALID("ENGAGEMENT  timetsamp: {} is invalid"),
    MSG_TEMPLATE_ENGAGEMENT_ID_EMPTY("ENGAGEMENT  id is empty"),
    MSG_TEMPLATE_ENGAGEMENT_CASCADE_LEVEL_EMPTY("ENGAGEMENT  cascadeLevel is empty"),
    MSG_TEMPLATE_ENGAGEMENT_EXISTENCE_BY_BOOKING_ID_AND_TOKEN("Checking Association existence by booking id: {}, and token number: {}"),
    MSG_TEMPLATE_ENGAGEMENT_EXISTS_BY_BOOKING_ID_AND_TOKEN("Association exists between booking id: {}, and token number: {}"),
    MSG_TEMPLATE_ENGAGEMENT_NON_EXISTENCE_BY_BOOKING_ID_AND_TOKEN("No Association exists between booking id: {}, and token number: {}");

    private String value;

    private EngagementMessageTemplate(String value) {
        this.value = value;
    }


}
