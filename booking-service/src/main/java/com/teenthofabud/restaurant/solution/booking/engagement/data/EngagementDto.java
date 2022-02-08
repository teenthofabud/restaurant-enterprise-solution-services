package com.teenthofabud.restaurant.solution.booking.engagement.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class EngagementDto {

    @ToString.Include
    private Optional<String> associationId;
    @ToString.Include
    private Optional<String> event;
    @ToString.Include
    private Optional<String> date;
    @ToString.Include
    private Optional<String> time;
    @ToString.Include
    private Optional<String> active;

    public EngagementDto() {
        this.associationId = Optional.ofNullable(null);
        this.event = Optional.ofNullable(null);
        this.date = Optional.ofNullable(null);
        this.time = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

}
