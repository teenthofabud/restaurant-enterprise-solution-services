package com.teenthofabud.restaurant.solution.cookbook.ingredient.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.data.ProductVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class IngredientVo extends TOABBaseVo implements Comparable<IngredientVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    private String productId;
    @ToString.Include
    private ProductVo product;
    @ToString.Include
    private String quantity;
    @ToString.Include
    private String recipeId;
    @ToString.Include
    private RecipeVo recipe;

    @Override
    public int compareTo(IngredientVo o) {
        int recipe = this.getRecipe() != null ? this.getRecipe().compareTo(o.getRecipe()) : this.getRecipeId().compareTo(o.getRecipeId());
        int product = this.getProduct() != null ? this.getProduct().compareTo(o.getProduct()) : this.getProductId().compareTo(o.getProductId());
        return Integer.compare(Integer.compare(recipe, product), this.getId().compareTo(o.getId()));
    }
}
