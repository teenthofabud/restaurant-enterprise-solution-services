package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class ReservationDto implements CheckInDtoDetails {

    @ToString.Include
    private Optional<String> date;
    @ToString.Include
    private Optional<String> time;

    public ReservationDto() {
        this.date = Optional.ofNullable(null);
        this.time = Optional.ofNullable(null);
    }

}
