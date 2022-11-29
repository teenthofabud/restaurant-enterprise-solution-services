package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.integration.customer.data.AccountVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true, callSuper = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class CheckInVo extends TOABBaseVo implements Comparable<CheckInVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String sequence;
    @ToString.Include
    private Integer noOfPersons;
    @ToString.Include
    private String accountId;
    @ToString.Include
    private AccountVo account;
    /*
    @ToString.Include
    private String status;*/
    @ToString.Include
    private String notes;
    @ToString.Include
    private String type;


    @Override
    public int compareTo(CheckInVo o) {
        return Integer.compare(this.getCreatedOn().compareTo(o.getCreatedOn()), this.getSequence().compareTo(o.getSequence()));
    }

    public CheckInVo(CheckInVo vo) {
        super.setCreatedBy(vo.getCreatedBy());
        super.setModifiedBy(vo.getModifiedBy());
        super.setCreatedOn(vo.getCreatedOn());
        super.setModifiedOn(vo.getModifiedOn());
        super.setActive(vo.getActive());
        this.account = vo.getAccount();
        this.accountId = vo.getAccountId();
        this.type = vo.getType();
        this.id = vo.getId();
        this.noOfPersons = vo.getNoOfPersons();
        this.notes = vo.getNotes();
        this.sequence = vo.getSequence();
    }
}
