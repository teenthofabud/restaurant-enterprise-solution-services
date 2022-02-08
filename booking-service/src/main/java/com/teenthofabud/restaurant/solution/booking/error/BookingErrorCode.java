package com.teenthofabud.restaurant.solution.booking.error;

import com.teenthofabud.core.common.error.TOABError;

public enum BookingErrorCode implements TOABError {

    BOOKING_ATTRIBUTE_INVALID("RES-BOOKING-001", 400), // syntactic
    BOOKING_NOT_FOUND("RES-BOOKING-002", 404),
    BOOKING_ATTRIBUTE_UNEXPECTED("RES-BOOKING-003", 422), // semantic
    BOOKING_EXISTS("RES-BOOKING-004", 409),
    BOOKING_INACTIVE("RES-BOOKING-005", 400),
    BOOKING_ACTION_FAILURE("RES-BOOKING-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private BookingErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "BookingErrorCode{" +
                this.name() + " -> " +
                "errorCode='" + errorCode + '\'' +
                ", httpStatusCode=" + httpStatusCode +
                '}';
    }

    @Override
    public String getErrorCode() {
        return this.errorCode;
    }

    @Override
    public Integer getHttpStatusCode() {
        return this.httpStatusCode;
    }

    @Override
    public String getDomain() {
        return "Booking";
    }

}
