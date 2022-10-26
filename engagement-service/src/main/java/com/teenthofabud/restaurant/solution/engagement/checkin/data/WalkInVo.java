package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class WalkInVo extends CheckInVo {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String name;
    @EqualsAndHashCode.Include
    @ToString.Include
    private String phoneNumber;
    @EqualsAndHashCode.Include
    @ToString.Include
    private String emailId;

    public WalkInVo(CheckInVo vo) {
        super(vo);
        this.name = "";
        this.emailId = "";
        this.phoneNumber = "";
    }

}
