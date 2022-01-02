package com.teenthofabud.restaurant.solution.inventory.category.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;


@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class CategoryForm {

    @ToString.Include
    private String name;
    @ToString.Include
    private String description;

}
