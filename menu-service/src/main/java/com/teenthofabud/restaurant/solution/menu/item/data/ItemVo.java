package com.teenthofabud.restaurant.solution.menu.item.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class ItemVo extends TOABBaseVo implements Comparable<ItemVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    private VegeterianStatus isVegeterian;
    @ToString.Include
    private String imageUrl;
    @ToString.Include
    private String categoryId;
    @ToString.Include
    private CategoryVo category;

    @Override
    public int compareTo(ItemVo o) {
        return this.getName().compareTo(o.getName());
    }
}
