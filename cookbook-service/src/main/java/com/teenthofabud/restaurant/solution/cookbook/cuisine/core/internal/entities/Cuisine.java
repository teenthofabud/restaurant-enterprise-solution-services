package com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.entities;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class Cuisine {

    private Long id;
    private String name;
    private String description;
    private Boolean active;

}
