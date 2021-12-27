package com.teenthofabud.restaurant.solution.establishmentarea.table.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class TableForm {

    @ToString.Include
    private String tableName;

    @ToString.Include
    private String description;

    @ToString.Include
    private String capacity;

    @ToString.Include
    private String floorId;

}
