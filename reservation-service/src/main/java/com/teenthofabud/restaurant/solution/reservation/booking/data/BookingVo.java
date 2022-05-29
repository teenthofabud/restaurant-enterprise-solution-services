package com.teenthofabud.restaurant.solution.reservation.booking.data;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateSerializer;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.reservation.integration.customer.data.AccountVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class BookingVo extends TOABBaseVo implements Comparable<BookingVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String categoryId;
    @ToString.Include
    private CategoryVo category;
    /*@ToString.Include
    private Integer noOfPerson;*/
    @ToString.Include
    private String accountId;
    @ToString.Include
    private AccountVo account;
    @JsonSerialize(using = LocalDateSerializer.class)
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy~HH:mm:ss")
    @ToString.Include
    private String timestamp;

    @Override
    public int compareTo(BookingVo o) {
        return Integer.compare(this.timestamp.compareTo(o.getTimestamp()),
                this.account == null ? this.accountId.compareTo(o.getAccountId()) : this.account.getId().compareTo(o.getAccount().getId()));
    }
}
