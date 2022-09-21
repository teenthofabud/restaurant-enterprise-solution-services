package com.teenthofabud.restaurant.solution.engagement.engagement.data;

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
    private Optional<String> bookingId;
    @ToString.Include
    private Optional<String> tokenNumber;
    // START -> DINE IN
    @ToString.Include
    private Optional<String> noOfPersons;
    @ToString.Include
    private Optional<String> tableId;
    // END -> DINE IN
    // START -> DELIVERY
    @ToString.Include
    private Optional<String> extRef;
    // END -> DELIVERY
    // START -> INSTRUCTIONS
    @ToString.Include
    private Optional<String> instructions;
    // END -> INSTRUCTIONS
    @ToString.Include
    private Optional<String> active;

    public EngagementDto() {
        this.bookingId = Optional.ofNullable(null);
        this.tokenNumber = Optional.ofNullable(null);
        this.noOfPersons = Optional.ofNullable(null);
        this.tableId = Optional.ofNullable(null);
        this.instructions = Optional.ofNullable(null);
        this.extRef = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

}
