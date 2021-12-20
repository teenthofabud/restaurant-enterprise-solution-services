package com.teenthofabud.restaurant.solution.settings.discount.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class DiscountForm {

    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    private Double rate;

    public DiscountForm() {
        this.name = null;
        this.description = null;
        this.rate = null;
    }

}
