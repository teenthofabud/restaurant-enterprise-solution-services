package com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;


@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class CuisineRequest {

    @ToString.Include
    private String name;
    @ToString.Include
    private String description;

}
