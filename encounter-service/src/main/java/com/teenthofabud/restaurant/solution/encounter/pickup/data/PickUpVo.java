package com.teenthofabud.restaurant.solution.encounter.pickup.data;

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
public class PickUpVo extends MeetingVo {

    @ToString.Include
    private String phoneNo;
    @ToString.Include
    private String name;

    public PickUpVo(MeetingVo vo) {
        super(vo);
        this.name = "";
        this.phoneNo = "";
    }
}
