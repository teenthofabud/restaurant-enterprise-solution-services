package com.teenthofabud.restaurant.solution.menu.price.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class PriceDto {

    @ToString.Include
    private Optional<String> amount;
    @ToString.Include
    private Optional<String> itemId;
    @ToString.Include
    private Optional<String> currencyId;
    @ToString.Include
    private Optional<String> active;


    public PriceDto() {
        this.amount = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
        this.itemId = Optional.ofNullable(null);
        this.currencyId = Optional.ofNullable(null);
    }

}
