package com.teenthofabud.restaurant.solution.customer.address.data;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class AddressForm {

    @JsonIgnore
    @ToString.Include
    private String name;
    @ToString.Include
    private String addressLine1;
    @ToString.Include
    private String addressLine2;
    @ToString.Include
    @JsonIgnore
    private String cityId;
    @ToString.Include
    private String stateId;
    @ToString.Include
    private String pincode;
    @ToString.Include
    private String countryId;
    @ToString.Include
    private String accountId;

    public AddressForm() {
        this.name = "default";
    }

}
