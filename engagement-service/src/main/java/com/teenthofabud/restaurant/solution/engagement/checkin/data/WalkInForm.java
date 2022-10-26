package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class WalkInForm extends CheckInForm {

    private String name;
    private String phoneNumber;
    private String emailId;

    public WalkInForm(CheckInForm form) {
        super(form);
        this.name = "";
        this.phoneNumber = "";
        this.emailId = "";
    }

}
