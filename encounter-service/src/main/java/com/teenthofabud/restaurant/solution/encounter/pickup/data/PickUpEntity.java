package com.teenthofabud.restaurant.solution.encounter.pickup.data;

import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;

@Getter
@Setter
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "pick_up")
@EntityListeners(AuditingEntityListener.class)
@Inheritance(
        strategy = InheritanceType.JOINED
)
@NoArgsConstructor
public class PickUpEntity extends MeetingEntity {

    @Column(name = "name")
    private String name;
    @Column(name = "phone_number")
    private String phoneNo;

    public PickUpEntity(MeetingEntity parent) {
        super(parent);
        this.phoneNo = "";
        this.name = "";
    }
}
