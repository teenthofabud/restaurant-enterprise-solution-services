package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.restaurant.solution.engagement.constants.CheckInType;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class CheckInForm {

    //private String tableId;
    private String accountId;
    private String sequence;
    private Integer noOfPersons;
    //private String status;
    /*private String name;
    private String phoneNumber;
    private String emailId;*/
    private String notes;

    private CheckInType type;
    private CheckInFormDetails attributes;


}
