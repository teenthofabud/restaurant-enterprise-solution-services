package com.teenthofabud.restaurant.solution.reservation.booking.data;

import lombok.Getter;

@Getter
public enum BookingMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_BOOKING_ENTITY_ID("Searching for BOOKING Document with id: {}"),
    MSG_TEMPLATE_NO_BOOKING_ENTITY_ID_AVAILABLE("No BOOKING Document available with id: {}"),
    MSG_TEMPLATE_FOUND_BOOKING_ENTITY_ID("Found BOOKING Document with id: {}"),
    MSG_TEMPLATE_BOOKING_ID_VALID("BOOKING  id: {} is semantically valid"),
    MSG_TEMPLATE_BOOKING_ID_INVALID("BOOKING  id: {} is invalid"),
    MSG_TEMPLATE_BOOKING_ID_EMPTY("BOOKING  id is empty"),
    MSG_TEMPLATE_BOOKING_CASCADE_LEVEL_EMPTY("BOOKING  cascadeLevel is empty"),
    MSG_TEMPLATE_BOOKING_EXISTENCE_BY_TIMESTAMP_AND_ACCOUNT_ID("Checking Booking existence by timestamp: {} and accountId: {}"),
    MSG_TEMPLATE_BOOKING_EXISTS_BY_TIMESTAMP_AND_ACCOUNT_ID("Booking exists between timestamp: {} and accountId: {}"),
    MSG_TEMPLATE_BOOKING_NON_EXISTENCE_BY_TIMESTAMP_AND_ACCOUNT_ID("No Booking exists between timestamp: {} and accountId: {}");

    private String value;

    private BookingMessageTemplate(String value) {
        this.value = value;
    }


}
