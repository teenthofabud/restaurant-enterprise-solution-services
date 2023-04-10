package com.teenthofabud.restaurant.solution.encounter.meeting.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class MeetingForm {

    private String accountId;
    private String sequence;

    public MeetingForm(MeetingForm form) {
        this.accountId = form.getAccountId();
        this.sequence = form.getSequence();
    }


}
