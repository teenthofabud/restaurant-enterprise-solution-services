package com.teenthofabud.restaurant.solution.establishmentarea.table.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class TableVo extends TOABBaseVo implements Comparable<TableVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String tableId;
    @ToString.Include
    private String tableName;
    @ToString.Include
    private String description;
    @ToString.Include
    private String capacity;
    @ToString.Include
    private String floorId;
    @ToString.Include
    private FloorVo floorVo;

    @Override
    public int compareTo(TableVo o) {
        return Integer.compare(this.getFloorVo() != null ? this.getFloorVo().compareTo(o.getFloorVo()) : this.getFloorId().compareTo(o.getFloorId()),
                this.getTableId().compareTo(o.getTableId()));
    }
}
