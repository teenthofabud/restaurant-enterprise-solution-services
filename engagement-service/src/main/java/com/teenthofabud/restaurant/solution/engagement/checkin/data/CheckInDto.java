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
public class CheckInDto {

    @ToString.Include
    private Optional<String> accountId;
    @ToString.Include
    private Optional<String> sequence;
    @ToString.Include
    private Optional<String> noOfPersons;
    /*@ToString.Include
    private Optional<String> status;*/
    @ToString.Include
    private Optional<String> notes;
    @ToString.Include
    private Optional<String> active;


    public CheckInDto() {
        this.noOfPersons = Optional.ofNullable(null);
        this.sequence = Optional.ofNullable(null);
        this.accountId = Optional.ofNullable(null);
        //this.status = Optional.ofNullable(null);
        this.notes = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

    public CheckInDto(CheckInDto dto) {
        this.noOfPersons = dto.getNoOfPersons();
        this.sequence = dto.getSequence();
        this.accountId = dto.getAccountId();
        //this.status = Optional.ofNullable(null);
        this.notes = dto.getNotes();
        this.active = dto.getActive();
    }

}
