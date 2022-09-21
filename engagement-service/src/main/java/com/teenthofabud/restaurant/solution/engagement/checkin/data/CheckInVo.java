package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateSerializer;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.engagement.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.engagement.integration.customer.data.AccountVo;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.data.TableVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public abstract class CheckInVo extends TOABBaseVo implements Comparable<CheckInVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    /*@ToString.Include
    private String tableId;
    @ToString.Include
    private TableVo table;*/
    @ToString.Include
    private String sequence;
    @ToString.Include
    private Integer noOfPersons;
    @ToString.Include
    private String accountId;
    @ToString.Include
    private AccountVo account;
    /*@ToString.Include
    private String name;
    @ToString.Include
    private String phoneNumber;
    @ToString.Include
    private String emailId;
    @ToString.Include
    private String status;*/
    @ToString.Include
    private String notes;
    @ToString.Include
    private CheckInVoDetails attributes;


    @Override
    public int compareTo(CheckInVo o) {
        return Integer.compare(this.getCreatedOn().compareTo(o.getCreatedOn()), this.getSequence().compareTo(o.getSequence()));
    }
}
