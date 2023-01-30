package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;
import org.springframework.util.CollectionUtils;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity(name = "checkIn")
@Table(name = "check_in")
@EntityListeners(AuditingEntityListener.class)
@Inheritance(
        strategy = InheritanceType.JOINED
)
public class CheckInEntity extends TOABBaseEntity implements Comparable<CheckInEntity> {

    public CheckInEntity(CheckInEntity checkInEntity) {
        this.id = checkInEntity.getId();
        this.accountId = checkInEntity.getAccountId();
        this.sequence = checkInEntity.getSequence();
        this.notes = checkInEntity.getNotes();
        this.noOfPersons = checkInEntity.getNoOfPersons();
        this.active = checkInEntity.getActive();
    }

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @ToString.Include
    private Long id;
    @Column(name = "customer_account_id")
    @ToString.Include
    private String accountId;
    @ToString.Include
    private String sequence;
    @Column(name = "number_of_persons")
    @ToString.Include
    private Integer noOfPersons;
    //private List<CheckInHistoryDocument> statusHistory;
    @ToString.Include
    private String notes;

    @OneToMany(mappedBy = "checkIn", fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    private List<TableAllocationEntity> tableAllocations;

    public void addTableAllocation(TableAllocationEntity tableAllocation) {
        if(CollectionUtils.isEmpty(this.tableAllocations)) {
            this.tableAllocations = new ArrayList<>();
        }
        this.tableAllocations.add(tableAllocation);
    }


    public Optional<TableAllocationEntity> getTableAllocation() {
        Optional<TableAllocationEntity> optStatus = Optional.empty();
        if(!CollectionUtils.isEmpty(this.tableAllocations)) {
            this.tableAllocations.sort(new Comparator<TableAllocationEntity>() {
                @Override
                public int compare(TableAllocationEntity o1, TableAllocationEntity o2) {
                    return o1.getCreatedOn().compareTo(o2.getCreatedOn());
                }
            });
            TableAllocationEntity tableAllocation = this.tableAllocations.get(this.tableAllocations.size() - 1);
            optStatus = Optional.of(tableAllocation);
        }
        return optStatus;
    }

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
    public int compareTo(CheckInEntity o) {
        return this.getId().compareTo(o.getId());
    }

}
