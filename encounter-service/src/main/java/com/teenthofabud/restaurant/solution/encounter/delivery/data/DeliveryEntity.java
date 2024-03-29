package com.teenthofabud.restaurant.solution.encounter.delivery.data;

import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;

@Getter
@Setter
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity(name = "delivery")
@Table(name = "delivery")
@EntityListeners(AuditingEntityListener.class)
@Inheritance(
        strategy = InheritanceType.JOINED
)
@PrimaryKeyJoinColumn(name = "encounter_meeting_id")
@NoArgsConstructor
public class DeliveryEntity extends MeetingEntity {

    @Column(name = "order_id")
    private String orderId;

    public DeliveryEntity(MeetingEntity parent) {
        super(parent);
        this.orderId = "";
    }
}
