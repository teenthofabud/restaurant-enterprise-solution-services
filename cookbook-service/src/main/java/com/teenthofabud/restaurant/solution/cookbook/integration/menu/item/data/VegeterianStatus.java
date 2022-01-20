package com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.data;

public enum VegeterianStatus {

    YES, NO;

    public static boolean getSwitchValue(VegeterianStatus vs) {
        return vs.compareTo(VegeterianStatus.YES) == 0 ? Boolean.TRUE : Boolean.FALSE;
    }

    public static VegeterianStatus getSwitchValue(Boolean sw) {
        return sw ? VegeterianStatus.YES : VegeterianStatus.NO;
    }

}
