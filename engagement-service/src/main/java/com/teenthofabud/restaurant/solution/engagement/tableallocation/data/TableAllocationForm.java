package com.teenthofabud.restaurant.solution.engagement.tableallocation.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class TableAllocationForm {

    private String tableId;
    //private String status;
    private String notes;

    private String checkInId;


    public TableAllocationForm(TableAllocationForm form) {
        this.tableId = form.getTableId();
        this.notes = form.getNotes();
        this.checkInId = form.getCheckInId();
    }

}
