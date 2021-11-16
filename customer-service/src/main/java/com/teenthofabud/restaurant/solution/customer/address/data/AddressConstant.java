package com.teenthofabud.restaurant.solution.customer.address.data;

import lombok.Getter;

@Getter
public enum AddressConstant {

    COUNTRY_ISO("countryIso");

    private String name;

    private AddressConstant(String name) {
        this.name = name;
    }

}
