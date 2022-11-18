package com.teenthofabud.restaurant.solution.encounter.meeting.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.encounter.integration.customer.data.AccountVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper=false)
public class MeetingVo extends TOABBaseVo implements Comparable<MeetingVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String sequence;
    @ToString.Include
    private String accountId;
    @ToString.Include
    private AccountVo accountVo;


    @Override
    public int compareTo(MeetingVo o) {
        return Integer.compare(this.getCreatedOn().compareTo(o.getCreatedOn()), this.getSequence().compareTo(o.getSequence()));
    }

    public MeetingVo(MeetingVo vo) {
        this.accountId = vo.getAccountId();
        this.accountVo = vo.getAccountVo();
        this.id = vo.getId();
        this.sequence = vo.getSequence();
    }
}
