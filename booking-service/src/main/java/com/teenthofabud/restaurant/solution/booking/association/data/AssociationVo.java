package com.teenthofabud.restaurant.solution.booking.association.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceVo;
import com.teenthofabud.restaurant.solution.booking.integration.customer.data.AccountVo;
import com.teenthofabud.restaurant.solution.booking.integration.establishmentarea.data.TableVo;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class AssociationVo extends TOABBaseVo implements Comparable<AssociationVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String experienceId;
    @ToString.Include
    private ExperienceVo experience;
    @ToString.Include
    private String tableId;
    @ToString.Include
    private TableVo table;
    @ToString.Include
    private String accountId;
    @ToString.Include
    private AccountVo account;
    @ToString.Include
    private LocalDateTime endedOn;

    @Override
    public int compareTo(AssociationVo o) {
        return Integer.compare(
                this.table == null ? this.tableId.compareTo(o.getTableId()) : this.table.getTableId().compareTo(o.getTable().getTableId()),
                this.account == null ? this.accountId.compareTo(o.getAccountId()) : this.account.getId().compareTo(o.getAccount().getId()));
    }
}
