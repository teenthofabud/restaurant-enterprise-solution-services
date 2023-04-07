package com.teenthofabud.restaurant.solution.encounter.meeting.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class MeetingDto {
    @ToString.Include
    private Optional<String> accountId;
    @ToString.Include
    private Optional<String> sequence;
    @ToString.Include
    private Optional<String> active;


    public MeetingDto() {
        this.sequence = Optional.ofNullable(null);
        this.accountId = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

    public MeetingDto(MeetingDto dto) {
        this.sequence = dto.getSequence();
        this.accountId = dto.getAccountId();
        this.active = dto.getActive();
    }

}
