package com.teenthofabud.restaurant.solution.reservation.error;

import com.teenthofabud.core.common.error.TOABError;

public enum ReservationErrorCode implements TOABError {

    RESERVATION_ATTRIBUTE_INVALID("RES-RESERVATION-001", 400), // syntactic
    RESERVATION_NOT_FOUND("RES-RESERVATION-002", 404),
    RESERVATION_ATTRIBUTE_UNEXPECTED("RES-RESERVATION-003", 422), // semantic
    RESERVATION_EXISTS("RES-RESERVATION-004", 409),
    RESERVATION_INACTIVE("RES-RESERVATION-005", 400),
    RESERVATION_ACTION_FAILURE("RES-RESERVATION-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private ReservationErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "ReservationErrorCode{" +
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
        return "Reservation";
    }

}
