package com.teenthofabud.restaurant.solution.engagement.constants;

import com.teenthofabud.core.common.error.TOABError;

public enum EngagementErrorCode implements TOABError {

    ENGAGEMENT_ATTRIBUTE_INVALID("RES-ENGMNT-001", 400), // syntactic
    ENGAGEMENT_NOT_FOUND("RES-ENGMNT-002", 404),
    ENGAGEMENT_ATTRIBUTE_UNEXPECTED("RES-ENGMNT-003", 422), // semantic
    ENGAGEMENT_EXISTS("RES-ENGMNT-004", 409),
    ENGAGEMENT_INACTIVE("RES-ENGMNT-005", 422),
    ENGAGEMENT_ACTION_FAILURE("RES-ENGMNT-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private EngagementErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "EngagementErrorCode{" +
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
        return "Engagement";
    }

}
