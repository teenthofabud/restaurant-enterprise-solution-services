package com.teenthofabud.restaurant.solution.cookbook.cuisine.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import lombok.*;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class CuisineVo extends TOABBaseVo implements Comparable<CuisineVo> {

    @ToString.Include
    private String id;
    @EqualsAndHashCode.Include
    @ToString.Include
    private String name;
    @EqualsAndHashCode.Include
    @ToString.Include
    private String description;
    @ToString.Include
    private List<RecipeVo> recipes;

    @Override
    public int compareTo(CuisineVo o) {
        return Integer.compare(this.getName().compareTo(o.getName()), this.getDescription().compareTo(o.getDescription()));
    }
}
