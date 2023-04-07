package com.teenthofabud.restaurant.solution.encounter.delivery.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class DeliveryForm extends MeetingForm {

    private String orderId;

    public DeliveryForm(MeetingForm form) {
        super(form);
        this.orderId = "";
    }
}
