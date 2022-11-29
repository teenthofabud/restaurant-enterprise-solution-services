package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ReservationForm extends CheckInForm {

    private String date;
    private String time;

    public ReservationForm(CheckInForm form) {
        super(form);
        this.date = "";
        this.time = "";
    }

}
