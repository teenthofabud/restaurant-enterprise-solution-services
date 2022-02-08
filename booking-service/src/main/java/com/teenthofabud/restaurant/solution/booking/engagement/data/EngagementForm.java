package com.teenthofabud.restaurant.solution.booking.engagement.data;

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
    private String associationId;
    @ToString.Include
    private String date;
    @ToString.Include
    private String time;
    @ToString.Include
    private String event;

}
