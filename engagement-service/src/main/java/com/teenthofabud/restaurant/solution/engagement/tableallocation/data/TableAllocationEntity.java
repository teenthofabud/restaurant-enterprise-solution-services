package com.teenthofabud.restaurant.solution.engagement.tableallocation.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity(name = "tableAllocation")
@Table(name = "table_allocation")
@EntityListeners(AuditingEntityListener.class)
public class TableAllocationEntity extends TOABBaseEntity implements Comparable<TableAllocationEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    @Column(name = "establishmentarea_table_id")
    private String tableId;
    //private List<CheckInHistoryDocument> statusHistory;
    private String notes;

    @ManyToOne(targetEntity = CheckInEntity.class, fetch = FetchType.LAZY, cascade = CascadeType.MERGE)
    @JoinColumn(name = "engagement_check_in_id")
    private CheckInEntity checkIn;

    /*public void addStatus(CheckInStatus status) {
        CheckInHistoryDocument checkInHistoryDocument = new CheckInHistoryDocument();
        checkInHistoryDocument.setStatus(status);
        this.statusHistory.add(checkInHistoryDocument);
    }

    public Optional<CheckInStatus> getStatus() {
        Optional<CheckInStatus> optStatus = Optional.empty();
        if(!CollectionUtils.isEmpty(this.statusHistory)) {
            this.statusHistory.sort(new Comparator<CheckInHistoryDocument>() {
                @Override
                public int compare(CheckInHistoryDocument o1, CheckInHistoryDocument o2) {
                    return o1.getCreatedOn().compareTo(o2.getCreatedOn());
                }
            });
            CheckInHistoryDocument checkInHistoryDocument = this.statusHistory.get(this.statusHistory.size() - 1);
            optStatus = Optional.of(checkInHistoryDocument.getStatus());
        }
        return optStatus;
    }*/

    @Override
    public int compareTo(TableAllocationEntity o) {
        return this.getId().compareTo(o.getId());
    }

}
