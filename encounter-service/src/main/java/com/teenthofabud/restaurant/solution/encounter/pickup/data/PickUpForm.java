package com.teenthofabud.restaurant.solution.encounter.pickup.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PickUpForm extends MeetingForm{

    private String name;
    private String phoneNo;

    public PickUpForm(MeetingForm form) {
        super(form);
        this.name = "";
        this.phoneNo = "";
    }


}
