package com.teenthofabud.restaurant.solution.cookbook.recipe.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class RecipeDto {

    @ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> description;
    @ToString.Include
    private Optional<String> instructions;
    @ToString.Include
    private Optional<String> numberOfServings;
    @ToString.Include
    private Optional<String> itemId;
    @ToString.Include
    private Optional<String> preparationTimeDuration;
    @ToString.Include
    private Optional<String> preparationTimeUnitId;
    @ToString.Include
    private Optional<String> cookingTimeDuration;
    @ToString.Include
    private Optional<String> cookingTimeUnitId;
    @ToString.Include
    private Optional<String> portionSizeAmount;
    @ToString.Include
    private Optional<String> portionSizeUnitId;
    @ToString.Include
    private Optional<String> cookingMethod;
    @ToString.Include
    private Optional<String> cuisineId;
    @ToString.Include
    private Optional<String> active;


    public RecipeDto() {
        this.name = Optional.ofNullable(null);
        this.description = Optional.ofNullable(null);
        this.instructions = Optional.ofNullable(null);
        this.numberOfServings = Optional.ofNullable(null);
        this.itemId = Optional.ofNullable(null);
        this.preparationTimeDuration = Optional.ofNullable(null);
        this.preparationTimeUnitId = Optional.ofNullable(null);
        this.cookingMethod = Optional.ofNullable(null);
        this.cookingTimeDuration = Optional.ofNullable(null);
        this.cookingTimeUnitId = Optional.ofNullable(null);
        this.portionSizeAmount = Optional.ofNullable(null);
        this.portionSizeUnitId = Optional.ofNullable(null);
        this.cuisineId = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

}
