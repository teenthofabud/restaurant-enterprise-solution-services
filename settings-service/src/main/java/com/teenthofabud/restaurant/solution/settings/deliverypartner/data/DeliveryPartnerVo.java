package com.teenthofabud.restaurant.solution.settings.deliverypartner.data;

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
public class DeliveryPartnerVo extends TOABBaseVo implements Comparable<DeliveryPartnerVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;

    @Override
    public int compareTo(DeliveryPartnerVo o) {
        return this.getName().compareTo(o.getName());
    }
}
