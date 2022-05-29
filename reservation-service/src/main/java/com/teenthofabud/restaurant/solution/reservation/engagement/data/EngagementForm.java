package com.teenthofabud.restaurant.solution.reservation.engagement.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class EngagementForm {

    @ToString.Include
    private String bookingId;
    @ToString.Include
    private String tokenNumber;
    @ToString.Include
    private Integer noOfPersons;
    @ToString.Include
    private String tableId;
    @ToString.Include
    private String instructions;
    @ToString.Include
    private String extRef;

}
