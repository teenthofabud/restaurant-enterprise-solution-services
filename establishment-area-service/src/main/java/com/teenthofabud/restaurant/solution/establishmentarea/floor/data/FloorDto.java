package com.teenthofabud.restaurant.solution.establishmentarea.floor.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString(onlyExplicitlyIncluded = true)
@AllArgsConstructor
public class FloorDto {

    @ToString.Include
    private Optional<String> flrName;

    public FloorDto() {
        this.flrName = Optional.ofNullable(null);
    }

}
