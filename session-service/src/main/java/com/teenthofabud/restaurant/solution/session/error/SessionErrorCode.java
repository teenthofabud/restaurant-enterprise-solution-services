package com.teenthofabud.restaurant.solution.session.error;

import com.teenthofabud.core.common.error.TOABError;

public enum SessionErrorCode implements TOABError {

    SESSION_ATTRIBUTE_INVALID("RES-SESSION-001", 400), // syntactic
    SESSION_NOT_FOUND("RES-SESSION-002", 404),
    SESSION_ATTRIBUTE_UNEXPECTED("RES-SESSION-003", 422), // semantic
    SESSION_EXISTS("RES-SESSION-004", 409),
    SESSION_INACTIVE("RES-SESSION-005", 400),
    SESSION_ACTION_FAILURE("RES-SESSION-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private SessionErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "SessionErrorCode{" +
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
        return "Session";
    }

}
