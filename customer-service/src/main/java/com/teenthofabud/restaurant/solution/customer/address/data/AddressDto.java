package com.teenthofabud.restaurant.solution.customer.address.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class AddressDto {

    @ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> addressLine1;
    @ToString.Include
    private Optional<String> addressLine2;
    @ToString.Include
    private Optional<String> cityId;
    @ToString.Include
    private Optional<String> pincode;
    @ToString.Include
    private Optional<String> stateId;
    @ToString.Include
    private Optional<String> countryId;
    @ToString.Include
    private Optional<String> accountId;
    @ToString.Include
    private Optional<String> active;


    public AddressDto() {
        this.name = Optional.ofNullable(null);
        this.addressLine1 = Optional.ofNullable(null);
        this.addressLine2 = Optional.ofNullable(null);
        this.cityId = Optional.ofNullable(null);
        this.pincode = Optional.ofNullable(null);
        this.stateId = Optional.ofNullable(null);
        this.countryId = Optional.ofNullable(null);
        this.accountId = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

}
