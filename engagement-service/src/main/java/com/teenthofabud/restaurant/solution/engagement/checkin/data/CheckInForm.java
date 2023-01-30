package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class CheckInForm {

    private String accountId;
    private String sequence;
    private Integer noOfPersons;
    //private String status;
    private String notes;
    private String type;

    public CheckInForm(CheckInForm form) {
        this.accountId = form.getAccountId();
        this.sequence = form.getSequence();
        this.noOfPersons = form.getNoOfPersons();
        this.notes = form.getNotes();
        this.type = form.getType();
    }

}
