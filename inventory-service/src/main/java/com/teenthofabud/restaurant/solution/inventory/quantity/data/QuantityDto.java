package com.teenthofabud.restaurant.solution.inventory.quantity.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class QuantityDto {

    @ToString.Include
    private Optional<String> amount;
    @ToString.Include
    private Optional<String> productId;
    @ToString.Include
    private Optional<String> weightId;
    @ToString.Include
    private Optional<String> active;


    public QuantityDto() {
        this.amount = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
        this.productId = Optional.ofNullable(null);
        this.weightId = Optional.ofNullable(null);
    }

}
