package com.teenthofabud.restaurant.solution.cookbook.integration.menu.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import lombok.*;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class CategoryVo extends TOABBaseVo implements Comparable<CategoryVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    private List<ItemVo> items;

    @Override
    public int compareTo(CategoryVo o) {
        return this.getName().compareTo(o.getName());
    }
}
