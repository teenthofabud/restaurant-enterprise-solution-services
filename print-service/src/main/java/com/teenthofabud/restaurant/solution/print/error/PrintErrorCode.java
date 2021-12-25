package com.teenthofabud.restaurant.solution.print.error;

import com.teenthofabud.core.common.error.TOABError;

public enum PrintErrorCode implements TOABError {

    PRINT_ATTRIBUTE_INVALID("RES-PRINT-001", 400), // syntactic
    PRINT_NOT_FOUND("RES-PRINT-002", 404),
    PRINT_ATTRIBUTE_UNEXPECTED("RES-PRINT-003", 422), // semantic
    PRINT_EXISTS("RES-PRINT-004", 409),
    PRINT_INACTIVE("RES-PRINT-005", 400),
    PRINT_ACTION_FAILURE("RES-PRINT-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private PrintErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "PrintErrorCode{" +
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
        return "Print";
    }

}
