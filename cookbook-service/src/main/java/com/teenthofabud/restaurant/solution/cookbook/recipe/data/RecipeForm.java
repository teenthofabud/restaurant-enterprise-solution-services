package com.teenthofabud.restaurant.solution.cookbook.recipe.data;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class RecipeForm {

    @JsonIgnore
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

    public RecipeForm() {
        this.name = "default";
    }

}
