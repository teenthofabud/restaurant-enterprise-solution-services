package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString(callSuper = true)
@AllArgsConstructor
public class ReservationDto extends CheckInDto {

    @ToString.Include
    private Optional<String> date;
    @ToString.Include
    private Optional<String> time;

    public ReservationDto() {
        this.date = Optional.ofNullable(null);
        this.time = Optional.ofNullable(null);
    }

    public ReservationDto(CheckInDto dto) {
        super(dto);
    }

}
