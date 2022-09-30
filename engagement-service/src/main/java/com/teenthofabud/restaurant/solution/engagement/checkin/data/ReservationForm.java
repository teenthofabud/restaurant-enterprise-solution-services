package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ReservationForm implements CheckInFormParameters {

    private String date;
    private String time;

}
