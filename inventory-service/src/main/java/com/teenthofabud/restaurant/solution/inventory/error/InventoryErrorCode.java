package com.teenthofabud.restaurant.solution.inventory.error;

import com.teenthofabud.core.common.error.TOABError;

public enum InventoryErrorCode implements TOABError {

    INVENTORY_ATTRIBUTE_INVALID("RES-INVENTORY-001", 400), // syntactic
    INVENTORY_NOT_FOUND("RES-INVENTORY-002", 404),
    INVENTORY_ATTRIBUTE_UNEXPECTED("RES-INVENTORY-003", 422), // semantic
    INVENTORY_EXISTS("RES-INVENTORY-004", 409),
    INVENTORY_INACTIVE("RES-INVENTORY-005", 400),
    INVENTORY_ACTION_FAILURE("RES-INVENTORY-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private InventoryErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "InventoryErrorCode{" +
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
        return "Inventory";
    }

}
