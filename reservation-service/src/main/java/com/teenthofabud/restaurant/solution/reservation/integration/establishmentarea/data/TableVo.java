package com.teenthofabud.restaurant.solution.reservation.integration.establishmentarea.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
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

    @Override
    public int compareTo(TableVo o) {
        return Integer.compare(this.getFloorId().compareTo(o.getFloorId()), this.getTableId().compareTo(o.getTableId()));
    }
}