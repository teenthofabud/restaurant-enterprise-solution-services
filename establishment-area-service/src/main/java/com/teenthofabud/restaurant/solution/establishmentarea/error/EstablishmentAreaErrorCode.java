package com.teenthofabud.restaurant.solution.establishmentarea.error;

import com.teenthofabud.core.common.error.TOABError;

public enum EstablishmentAreaErrorCode implements TOABError {

    ESTABLISHMENT_AREA_ATTRIBUTE_INVALID("RES-EAREA-001", 400), // syntactic
    ESTABLISHMENT_AREA_NOT_FOUND("RES-EAREA-002", 404),
    ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED("RES-EAREA-003", 422), // semantic
    ESTABLISHMENT_AREA_EXISTS("RES-EAREA-004", 409),
    ESTABLISHMENT_AREA_INACTIVE("RES-EAREA-005", 400),
    ESTABLISHMENT_AREA_ACTION_FAILURE("RES-EAREA-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private EstablishmentAreaErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "EstablishmentAreaErrorCode {" +
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
        return "EstablishmentArea";
    }

}
