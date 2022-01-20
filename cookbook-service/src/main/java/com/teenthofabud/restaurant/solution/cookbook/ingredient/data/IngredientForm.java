package com.teenthofabud.restaurant.solution.cookbook.ingredient.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class IngredientForm {

    @ToString.Include
    private String name;
    private String description;
    @ToString.Include
    private String productId;
    private Double quantityAmount;
    private String quantityUnitId;
    @ToString.Include
    private String recipeId;

}
