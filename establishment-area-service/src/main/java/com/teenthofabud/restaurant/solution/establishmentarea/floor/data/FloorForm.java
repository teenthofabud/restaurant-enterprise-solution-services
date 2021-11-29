package com.teenthofabud.restaurant.solution.establishmentarea.floor.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class FloorForm {

    @ToString.Include
    private String flrName;

}
