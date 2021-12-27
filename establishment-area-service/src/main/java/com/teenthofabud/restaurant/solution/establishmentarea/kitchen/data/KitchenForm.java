package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class KitchenForm {

    @ToString.Include
    private String kitchenName;

    @ToString.Include
    private String description;

    @ToString.Include
    private String floorId;

}
