package com.teenthofabud.restaurant.solution.menu.item.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class ItemDto {

    @ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> description;
    @ToString.Include
    private Optional<String> isVegeterian;
    @ToString.Include
    private Optional<String> imageUrl;
    @ToString.Include
    private Optional<String> categoryId;
    @ToString.Include
    private Optional<String> active;


    public ItemDto() {
        this.name = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
        this.description = Optional.ofNullable(null);
        this.isVegeterian = Optional.ofNullable(null);
        this.categoryId = Optional.ofNullable(null);
        this.imageUrl = Optional.ofNullable(null);
    }

}
