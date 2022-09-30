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
public class WalkInDto implements CheckInDtoParameters {

    @ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> phoneNumber;
    @ToString.Include
    private Optional<String> emailId;

    public WalkInDto() {
        this.name = Optional.ofNullable(null);
        this.phoneNumber = Optional.ofNullable(null);
        this.emailId = Optional.ofNullable(null);
    }

}
