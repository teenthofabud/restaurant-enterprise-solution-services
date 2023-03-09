package com.teenthofabud.restaurant.solution.engagement.checkin.constants;

public enum CheckInType {

    WALK_IN("WI"),
    RESERVATION("RE");

    private String code;

    private CheckInType(String code) {
        this.code = code;
    }

    public String getCode() {
        return this.code;
    }

}
