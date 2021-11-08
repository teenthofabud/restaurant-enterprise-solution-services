package com.teenthofabud.restaurant.solution.customer.account.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;


@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class AccountForm {

    @ToString.Include
    private String firstName;
    @ToString.Include
    private String lastName;
    @ToString.Include
    private String genderId;
    @ToString.Include
    private String dateOfBirth;

}
