package com.teenthofabud.restaurant.solution.menu.error;

import com.teenthofabud.core.common.error.TOABError;

public enum MenuErrorCode implements TOABError {

    MENU_ATTRIBUTE_INVALID("RES-MENU-001", 400), // syntactic
    MENU_NOT_FOUND("RES-MENU-002", 404),
    MENU_ATTRIBUTE_UNEXPECTED("RES-MENU-003", 422), // semantic
    MENU_EXISTS("RES-MENU-004", 409),
    MENU_INACTIVE("RES-MENU-005", 400),
    MENU_ACTION_FAILURE("RES-MENU-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private MenuErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "MenuErrorCode{" +
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
        return "Menu";
    }

}
