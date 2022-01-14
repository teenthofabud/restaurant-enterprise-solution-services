package com.teenthofabud.restaurant.solution.cookbook.ingredient.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class IngredientDto {

    @ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> description;
    @ToString.Include
    private Optional<String> productId;
    @ToString.Include
    private Optional<String> quantityAmount;
    @ToString.Include
    private Optional<String> quantityUnitId;
    @ToString.Include
    private Optional<String> recipeId;
    @ToString.Include
    private Optional<String> active;


    public IngredientDto() {
        this.name = Optional.ofNullable(null);
        this.description = Optional.ofNullable(null);
        this.productId = Optional.ofNullable(null);
        this.quantityAmount = Optional.ofNullable(null);
        this.quantityUnitId = Optional.ofNullable(null);
        this.recipeId = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

}
