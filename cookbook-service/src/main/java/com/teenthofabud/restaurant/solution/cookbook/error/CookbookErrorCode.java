package com.teenthofabud.restaurant.solution.cookbook.error;

import com.teenthofabud.core.common.error.TOABError;

public enum CookbookErrorCode implements TOABError {

    COOK_ATTRIBUTE_INVALID("RES-COOK-001", 400), // syntactic
    COOK_NOT_FOUND("RES-COOK-002", 404),
    COOK_ATTRIBUTE_UNEXPECTED("RES-COOK-003", 422), // semantic
    COOK_EXISTS("RES-COOK-004", 409),
    COOK_INACTIVE("RES-COOK-005", 400),
    COOK_ACTION_FAILURE("RES-COOK-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private CookbookErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "CookbookErrorCode{" +
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
        return "Cookbook";
    }

}
