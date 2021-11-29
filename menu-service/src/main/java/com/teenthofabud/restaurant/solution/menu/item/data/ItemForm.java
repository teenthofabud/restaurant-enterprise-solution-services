package com.teenthofabud.restaurant.solution.menu.item.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

import javax.persistence.Column;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ItemForm {

    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    private String isVegeterian;
    @ToString.Include
    private String imageUrl;
    @ToString.Include
    private String categoryId;

}
