package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import io.micrometer.core.instrument.ImmutableTag;
import lombok.Getter;

@Getter
public enum CheckInMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID("Searching for CHECKIN Document with id: {}"),
    MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE("No CHECKIN Document available with id: {}"),
    MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID("Found CHECKIN Document with id: {}"),
    MSG_TEMPLATE_CHECKIN_ID_VALID("CHECKIN  id: {} is semantically valid"),
    MSG_TEMPLATE_CHECKIN_ID_INVALID("CHECKIN  id: {} is invalid"),
    MSG_TEMPLATE_CHECKIN_ID_EMPTY("CHECKIN  id is empty"),
    MSG_TEMPLATE_CHECKIN_CASCADE_LEVEL_EMPTY("CHECKIN  cascadeLevel is empty"),
    MSG_TEMPLATE_CHECKIN_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE("Checking CheckIn existence by accountId: {} and sequence: {}"),
    MSG_TEMPLATE_CHECKIN_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE("CheckIn exists between accountId: {} and sequence: {}"),
    MSG_TEMPLATE_CHECKIN_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE("No CheckIn exists between accountId: {} and sequence: {}");

    private String value;

    private CheckInMessageTemplate(String value) {
        this.value = value;
    }


}
