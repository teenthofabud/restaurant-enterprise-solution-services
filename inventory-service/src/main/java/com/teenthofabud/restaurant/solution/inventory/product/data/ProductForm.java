package com.teenthofabud.restaurant.solution.inventory.product.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ProductForm {

    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    private String imageUrl;
    @ToString.Include
    private String categoryId;

}
