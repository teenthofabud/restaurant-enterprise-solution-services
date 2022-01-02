package com.teenthofabud.restaurant.solution.inventory.quantity.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class QuantityForm {

    @ToString.Include
    private String weightId;
    @ToString.Include
    private Double amount;
    @ToString.Include
    private String productId;

}
