package com.teenthofabud.restaurant.solution.inventory.product.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class ProductVo extends TOABBaseVo implements Comparable<ProductVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    private String imageUrl;
    @ToString.Include
    private String categoryId;
    @ToString.Include
    private CategoryVo category;

    @Override
    public int compareTo(ProductVo o) {
        return this.getName().compareTo(o.getName());
    }
}
