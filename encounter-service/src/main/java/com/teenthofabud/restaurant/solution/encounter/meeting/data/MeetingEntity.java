package com.teenthofabud.restaurant.solution.encounter.meeting.data;

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
@Entity(name = "meeting")
@Table(name = "meeting")
@EntityListeners(AuditingEntityListener.class)
@Inheritance(
        strategy = InheritanceType.JOINED
)
public class MeetingEntity extends TOABBaseEntity implements Comparable<MeetingEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    @Column(name = "account_id")
    private String accountId;
    @Column(name = "sequence")
    private String sequence;

    public MeetingEntity() {
        this.id = 0L;
        this.accountId = "";
        this.sequence = "";
    }

    public MeetingEntity(MeetingEntity meetingEntity) {
        this.id = meetingEntity.getId();
        this.accountId = meetingEntity.getAccountId();
        this.sequence = meetingEntity.getSequence();
    }

    @Override
    public int compareTo(MeetingEntity o) {
        return this.getId().compareTo(o.getId());
    }

}
