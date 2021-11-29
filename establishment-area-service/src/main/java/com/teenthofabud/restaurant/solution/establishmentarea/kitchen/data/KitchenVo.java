package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data;

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
public class KitchenVo extends TOABBaseVo implements Comparable<KitchenVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String kitchenId;
    @ToString.Include
    private String kitchenName;
    @ToString.Include
    private String description;

    @Override
    public int compareTo(KitchenVo o) {
        return this.getKitchenName().compareTo(o.getKitchenName());
    }
}
