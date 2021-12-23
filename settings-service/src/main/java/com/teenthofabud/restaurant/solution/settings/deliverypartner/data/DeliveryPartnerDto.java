package com.teenthofabud.restaurant.solution.settings.deliverypartner.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class DeliveryPartnerDto {

    @ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> description;
    @ToString.Include
    private Optional<String> active;

    public DeliveryPartnerDto() {
        this.name = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
        this.description = Optional.ofNullable(null);
    }

}
