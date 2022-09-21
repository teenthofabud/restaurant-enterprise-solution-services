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
public class ReservationVo implements CheckInVoDetails {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String date;
    @EqualsAndHashCode.Include
    @ToString.Include
    private String time;

}
