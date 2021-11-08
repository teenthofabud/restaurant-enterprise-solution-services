package com.teenthofabud.restaurant.solution.customer.error;

import com.teenthofabud.core.common.error.TOABError;

public enum CustomerErrorCode implements TOABError {

    CUST_ATTRIBUTE_INVALID("RES-CUST-001", 400), // syntactic
    CUST_NOT_FOUND("RES-CUST-002", 404),
    CUST_ATTRIBUTE_UNEXPECTED("RES-CUST-003", 422), // semantic
    CUST_EXISTS("RES-CUST-004", 409),
    CUST_INACTIVE("RES-CUST-005", 400),
    CUST_ACTION_FAILURE("RES-CUST-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private CustomerErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "CustomerErrorCode{" +
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
        return "Customer";
    }

}
