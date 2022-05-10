package com.teenthofabud.restaurant.solution.reservation.booking.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class BookingForm {

    @ToString.Include
    private String categoryId;
    @ToString.Include
    private String timestamp;
   /* @ToString.Include
    private Integer noOfPerson;*/
    @ToString.Include
    private String accountId;

}
