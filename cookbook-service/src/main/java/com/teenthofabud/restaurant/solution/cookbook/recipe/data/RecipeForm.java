package com.teenthofabud.restaurant.solution.cookbook.recipe.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class RecipeForm {

    @ToString.Include
    private String name;
    private String description;
    private String instructions;
    private Integer numberOfServings;
    @ToString.Include
    private String itemId;
    private Double preparationTimeDuration;
    private String preparationTimeUnitId;
    private Double cookingTimeDuration;
    private String cookingTimeUnitId;
    private Double portionSizeAmount;
    private String portionSizeUnitId;
    private String cookingMethod;
    @ToString.Include
    private String cuisineId;

}
