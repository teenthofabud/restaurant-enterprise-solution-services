package com.teenthofabud.restaurant.solution.establishmentarea.table.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString(onlyExplicitlyIncluded = true)
@AllArgsConstructor
public class TableDto {

    @ToString.Include
    private Optional<String> tableName;
    @ToString.Include
    private Optional<String> description;
    @ToString.Include
    private Optional<String> capacity;
    @ToString.Include
    private Optional<String> floorId;
    @ToString.Include
    private Optional<String> active;

    public TableDto() {
        this.tableName = Optional.ofNullable(null);
        this.description = Optional.ofNullable(null);
        this.capacity = Optional.ofNullable(null);
        this.floorId = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }
}
