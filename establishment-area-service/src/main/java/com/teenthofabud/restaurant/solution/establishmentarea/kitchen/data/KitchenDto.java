package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString(onlyExplicitlyIncluded = true)
@AllArgsConstructor
public class KitchenDto {

    @ToString.Include
    private Optional<String> kitchenName;

    @ToString.Include
    private Optional<String> description;

    public KitchenDto() {
        this.kitchenName = Optional.ofNullable(null);
        this.description = Optional.ofNullable(null);
    }
}
