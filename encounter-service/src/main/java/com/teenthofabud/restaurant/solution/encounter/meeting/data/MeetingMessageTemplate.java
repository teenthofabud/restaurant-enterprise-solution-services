package com.teenthofabud.restaurant.solution.encounter.meeting.data;

import lombok.Getter;

@Getter
public enum MeetingMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_MEETING_ENTITY_ID("Searching for MEETING Document with id: {}"),
    MSG_TEMPLATE_NO_MEETING_ENTITY_ID_AVAILABLE("No MEETING Document available with id: {}"),
    MSG_TEMPLATE_FOUND_MEETING_ENTITY_ID("Found MEETING Document with id: {}"),
    MSG_TEMPLATE_MEETING_ID_VALID("MEETING  id: {} is semantically valid"),
    MSG_TEMPLATE_MEETING_ID_INVALID("MEETING  id: {} is invalid"),
    MSG_TEMPLATE_MEETING_ID_EMPTY("MEETING  id is empty"),
    MSG_TEMPLATE_MEETING_CASCADE_LEVEL_EMPTY("MEETING  cascadeLevel is empty"),
    MSG_TEMPLATE_MEETING_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE("Checking Meeting existence by accountId: {} and sequence: {}"),
    MSG_TEMPLATE_MEETING_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE("Meeting exists between accountId: {} and sequence: {}"),
    MSG_TEMPLATE_MEETING_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE("No Meeting exists between accountId: {} and sequence: {}");

    private String value;

    private MeetingMessageTemplate(String value) {
        this.value = value;
    }


}
