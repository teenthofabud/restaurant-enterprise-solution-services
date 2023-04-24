package com.teenthofabud.restaurant.solution.cookbook.recipe.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.data.ItemVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class RecipeVo extends TOABBaseVo implements Comparable<RecipeVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    private String instructions;
    @ToString.Include
    private Integer numberOfServings;
    @ToString.Include
    private String itemId;
    @ToString.Include
    private ItemVo item;
    @ToString.Include
    private String preparationTime;
    @ToString.Include
    private String cookingTime;
    @ToString.Include
    private String cookingMethod;
    @ToString.Include
    private String portionSize;
    @ToString.Include
    private String cuisineId;
    @ToString.Include
    private CuisineVo cuisine;

    @Override
    public int compareTo(RecipeVo o) {
        int cuisine = this.getCuisine() != null ? this.getCuisine().compareTo(o.getCuisine()) : this.getCuisineId().compareTo(o.getCuisineId());
        int item = this.getItem() != null ? this.getItem().compareTo(o.getItem()) : this.getItemId().compareTo(o.getItemId());
        return Integer.compare(Integer.compare(cuisine, item), this.getId().compareTo(o.getId()));
    }
}
