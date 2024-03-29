package com.teenthofabud.restaurant.solution.customer.account.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class AccountDto {

    @ToString.Include
    private Optional<String> firstName;
    @ToString.Include
    private Optional<String> lastName;
    @ToString.Include
    private Optional<String> genderId;
    @ToString.Include
    private Optional<String> dateOfBirth;
    @ToString.Include
    private Optional<String> active;
    @ToString.Include
    private Optional<String> phoneNumber;
    @ToString.Include
    private Optional<String> countryCode;
    @ToString.Include
    private Optional<String> emailId;


    public AccountDto() {
        this.firstName = Optional.ofNullable(null);
        this.lastName = Optional.ofNullable(null);
        this.genderId = Optional.ofNullable(null);
        this.dateOfBirth = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
        this.phoneNumber = Optional.ofNullable(null);
        this.countryCode = Optional.ofNullable(null);
        this.emailId = Optional.ofNullable(null);
    }

}
