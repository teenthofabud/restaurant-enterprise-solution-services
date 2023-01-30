package com.teenthofabud.restaurant.solution.encounter.constants;

import com.teenthofabud.core.common.error.TOABError;

public enum EncounterErrorCode implements TOABError {

    ENCOUNTER_ATTRIBUTE_INVALID("RES-ENCOUNTER-001", 400), // syntactic
    ENCOUNTER_NOT_FOUND("RES-ENCOUNTER-002", 404),
    ENCOUNTER_ATTRIBUTE_UNEXPECTED("RES-ENCOUNTER-003", 422), // semantic
    ENCOUNTER_EXISTS("RES-ENCOUNTER-004", 409),
    ENCOUNTER_INACTIVE("RES-ENCOUNTER-005", 400),
    ENCOUNTER_ACTION_FAILURE("RES-ENCOUNTER-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private EncounterErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "EncounterErrorCode{" +
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
        return "Encounter";
    }

}
