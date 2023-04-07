package com.teenthofabud.restaurant.solution.engagement.tableallocation.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class TableAllocationDto {

    @ToString.Include
    private Optional<String> tableId;
    @ToString.Include
    private Optional<String> checkInId;
    /*@ToString.Include
    private Optional<String> status;*/
    @ToString.Include
    private Optional<String> notes;
    @ToString.Include
    private Optional<String> active;


    public TableAllocationDto() {
        this.tableId = Optional.ofNullable(null);
        //this.status = Optional.ofNullable(null);
        this.notes = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
        this.checkInId = Optional.ofNullable(null);
    }

    public TableAllocationDto(TableAllocationDto dto) {
        this.tableId = Optional.ofNullable(null);
        //this.status = Optional.ofNullable(null);
        this.notes = dto.getNotes();
        this.active = dto.getActive();
        this.checkInId = dto.getCheckInId();
    }

}
