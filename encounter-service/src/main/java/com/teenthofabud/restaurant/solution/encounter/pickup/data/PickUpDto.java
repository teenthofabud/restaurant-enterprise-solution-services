package com.teenthofabud.restaurant.solution.encounter.pickup.data;

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
public class PickUpDto extends MeetingDto {
    @ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> phoneNo;
    @ToString.Include
    private Optional<String> active;


    public PickUpDto() {
        this.phoneNo = Optional.ofNullable(null);
        this.name = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

    public PickUpDto(MeetingDto dto) {
        super(dto);
    }
}
