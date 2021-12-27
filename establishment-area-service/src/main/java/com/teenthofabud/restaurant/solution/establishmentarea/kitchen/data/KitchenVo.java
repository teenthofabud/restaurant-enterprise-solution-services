package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data;

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
public class KitchenVo extends TOABBaseVo implements Comparable<KitchenVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String kitchenId;
    @ToString.Include
    private String kitchenName;
    @ToString.Include
    private String description;
    @ToString.Include
    private String floorId;
    @ToString.Include
    private FloorVo floorVo;

    @Override
    public int compareTo(KitchenVo o) {
        return Integer.compare(this.getFloorVo() != null ? this.getFloorVo().compareTo(o.getFloorVo()) : this.getFloorId().compareTo(o.getFloorId()),
                this.getKitchenId().compareTo(o.getKitchenId()));
    }
}
