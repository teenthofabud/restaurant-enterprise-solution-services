package com.teenthofabud.restaurant.solution.settings.charge.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ChargeForm {

    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    private Double rate;

    public ChargeForm() {
        this.name = null;
        this.description = null;
        this.rate = null;
    }

}
