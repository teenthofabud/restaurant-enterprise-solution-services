package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;

@Getter
@Setter
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "check_in")
@EntityListeners(AuditingEntityListener.class)
@Inheritance(
        strategy = InheritanceType.JOINED
)
public class CheckInEntity extends TOABBaseEntity implements Comparable<CheckInEntity> {

    public CheckInEntity() {
        this.id = 0l;
        //this.tableId = "";
        this.accountId = "";
        this.sequence = "";
        this.noOfPersons = 0;
        //this.statusHistory = new LinkedList<>();
        this.notes = "";
    }

    public CheckInEntity(CheckInEntity checkInEntity) {
        this.id = checkInEntity.getId();
        this.accountId = checkInEntity.getAccountId();
        this.sequence = checkInEntity.getSequence();
        this.notes = checkInEntity.getNotes();
        this.noOfPersons = checkInEntity.getNoOfPersons();
    }

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    /*@Indexed
    private List<String> tableId;*/
    @Column(name = "customer_account_id")
    private String accountId;
    private String sequence;
    @Column(name = "number_of_persons")
    private Integer noOfPersons;
    //private List<CheckInHistoryDocument> statusHistory;
    private String notes;

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
