package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true, callSuper = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class ReservationVo extends CheckInVo {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String date;
    @EqualsAndHashCode.Include
    @ToString.Include
    private String time;

    public ReservationVo(CheckInVo vo) {
        super(vo);
        this.date = "";
        this.time = "";
    }

}
