package com.teenthofabud.restaurant.solution.engagement.integration.customer.data;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateSerializer;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import lombok.*;

import java.time.LocalDate;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class AccountVo extends TOABBaseVo implements Comparable<AccountVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String firstName;
    @ToString.Include
    private String lastName;
    @ToString.Include
    private String genderId;
    @JsonSerialize(using = LocalDateSerializer.class)
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy") // date pattern referenced from res.customer.dob.format
    @ToString.Include
    private LocalDate dateOfBirth;
    @ToString.Include
    private String phoneNumber;
    @ToString.Include
    private String countryCode;
    @ToString.Include
    private String emailId;

    @Override
    public int compareTo(AccountVo o) {
        return Integer.compare(this.getFirstName().compareTo(o.getFirstName()), this.getLastName().compareTo(o.getLastName()));
    }
}