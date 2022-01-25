package com.teenthofabud.restaurant.solution.establishmentarea.floor.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenVo;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableVo;
import lombok.*;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class FloorVo extends TOABBaseVo implements Comparable<FloorVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String flrId;
    @ToString.Include
    private String flrName;
    @ToString.Include
    private List<KitchenVo> kitchens;
    @ToString.Include
    private List<TableVo> tables;

    @Override
    public int compareTo(FloorVo o) {
        return this.getFlrName().compareTo(o.getFlrName());
    }
}
