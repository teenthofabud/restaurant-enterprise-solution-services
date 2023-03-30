package com.teenthofabud.restaurant.solution.engagement.tableallocation.data;

import lombok.*;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
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


    /*public TableAllocationDto() {
        this.tableId = Optional.empty();
        //this.status = Optional.ofNullable(null);
        this.notes = Optional.empty();
        this.active = Optional.empty();
        this.checkInId = Optional.empty();
    }*/

    public TableAllocationDto(TableAllocationDto dto) {
        this.tableId = dto.getTableId();
        //this.status = Optional.ofNullable(null);
        this.notes = dto.getNotes();
        this.active = dto.getActive();
        this.checkInId = dto.getCheckInId();
    }

}
