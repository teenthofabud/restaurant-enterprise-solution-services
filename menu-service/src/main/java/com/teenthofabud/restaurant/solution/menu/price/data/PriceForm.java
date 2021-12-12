package com.teenthofabud.restaurant.solution.menu.price.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PriceForm {

    @ToString.Include
    private String currencyId;
    @ToString.Include
    private Double amount;
    @ToString.Include
    private String itemId;

}
