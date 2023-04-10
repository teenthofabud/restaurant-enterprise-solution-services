package com.teenthofabud.restaurant.solution.encounter.delivery.data;

import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingDto;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class DeliveryDto extends MeetingDto{
    @ToString.Include
    private Optional<String> orderId;
    @ToString.Include
    private Optional<String> active;


    public DeliveryDto() {
        this.orderId = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

    public DeliveryDto(MeetingDto dto) {
        super(dto);
    }
}
