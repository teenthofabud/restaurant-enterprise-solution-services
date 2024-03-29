package com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.data;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
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
    private Integer capacity;
    @ToString.Include
    private String floorId;
    @ToString.Include
    private Boolean active;

    @Override
    public int compareTo(TableVo o) {
        return Integer.compare(this.getFloorId().compareTo(o.getFloorId()), this.getTableId().compareTo(o.getTableId()));
    }
}