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

    /*@ToString.Include
    private Optional<String> tableId;*/
    @ToString.Include
    private Optional<String> accountId;
    @ToString.Include
    private Optional<String> sequence;
    @ToString.Include
    private Optional<Integer> noOfPersons;
    /*@ToString.Include
    private Optional<String> status;*/
    /*@ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> phoneNumber;
    @ToString.Include
    private Optional<String> emailId;*/
    @ToString.Include
    private Optional<String> notes;
    @ToString.Include
    private Optional<String> active;
    @ToString.Include
    private Optional<CheckInDtoParameters> attributes;


    public CheckInDto() {
        //this.tableId = Optional.ofNullable(null);
        this.noOfPersons = Optional.ofNullable(null);
        this.sequence = Optional.ofNullable(null);
        this.accountId = Optional.ofNullable(null);
        //this.status = Optional.ofNullable(null);
        //this.name = Optional.ofNullable(null);
        //this.phoneNumber = Optional.ofNullable(null);
        //this.emailId = Optional.ofNullable(null);
        this.notes = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

}
