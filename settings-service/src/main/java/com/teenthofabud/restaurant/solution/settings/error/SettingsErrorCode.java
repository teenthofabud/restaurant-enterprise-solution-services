package com.teenthofabud.restaurant.solution.settings.error;

import com.teenthofabud.core.common.error.TOABError;

public enum SettingsErrorCode implements TOABError {

    SETTINGS_ATTRIBUTE_INVALID("RES-SETTINGS-001", 400), // syntactic
    SETTINGS_NOT_FOUND("RES-SETTINGS-002", 404),
    SETTINGS_ATTRIBUTE_UNEXPECTED("RES-SETTINGS-003", 422), // semantic
    SETTINGS_EXISTS("RES-SETTINGS-004", 409),
    SETTINGS_INACTIVE("RES-SETTINGS-005", 400),
    SETTINGS_ACTION_FAILURE("RES-SETTINGS-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private SettingsErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "SettingsErrorCode{" +
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
        return "Settings";
    }

}
