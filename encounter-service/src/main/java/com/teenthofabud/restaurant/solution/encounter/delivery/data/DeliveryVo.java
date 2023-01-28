package com.teenthofabud.restaurant.solution.encounter.delivery.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper=false)
public class DeliveryVo extends MeetingVo {

    @ToString.Include
    private String orderId;

    public DeliveryVo(MeetingVo vo) {
        super(vo);
        this.orderId = "";
    }
}
