package com.teenthofabud.restaurant.solution.engagement.booking.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class BookingDto {

    @ToString.Include
    private Optional<String> categoryId;
    @ToString.Include
    private Optional<String> timestamp;
    @ToString.Include
    private Optional<String> accountId;
    /*@ToString.Include
    private Optional<String> noOfPerson;*/
    @ToString.Include
    private Optional<String> active;

    public BookingDto() {
        this.categoryId = Optional.ofNullable(null);
        this.timestamp = Optional.ofNullable(null);
        this.accountId = Optional.ofNullable(null);
        //this.noOfPerson = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

}
