package com.teenthofabud.restaurant.solution.engagement.tableallocation.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInVo;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.data.TableVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class TableAllocationVo extends TOABBaseVo implements Comparable<TableAllocationVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String tableId;
    @ToString.Include
    private TableVo table;
    @ToString.Include
    private String checkInId;
    @ToString.Include
    private CheckInVo checkIn;
    /*
    @ToString.Include
    private String status;*/
    @ToString.Include
    private String notes;

    @Override
    public int compareTo(TableAllocationVo o) {
        return Integer.compare(this.getCreatedOn().compareTo(o.getCreatedOn()), this.getTableId().compareTo(o.getTableId()));
    }

    public TableAllocationVo(TableAllocationVo vo) {
        this.table = vo.getTable();
        this.tableId = vo.getTableId();
        this.id = vo.getId();
        this.notes = vo.getNotes();
        this.checkIn = vo.getCheckIn();
    }
}
