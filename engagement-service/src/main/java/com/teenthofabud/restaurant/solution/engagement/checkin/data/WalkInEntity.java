package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;


@Getter
@Setter
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true, callSuper = true)
@Entity(name = "walkIn")
@Table(name = "walk_in")
@EntityListeners(AuditingEntityListener.class)
@PrimaryKeyJoinColumn(name = "engagement_check_in_id")
@NoArgsConstructor
public class WalkInEntity extends CheckInEntity {

    public WalkInEntity(CheckInEntity parent) {
        super(parent);
        this.name = "";
        this.phoneNumber = "";
        this.emailId = "";
    }

    @ToString.Include
    private String name;
    @ToString.Include
    @Column(name = "phone_number")
    private String phoneNumber;
    @ToString.Include
    @Column(name = "email_id")
    private String emailId;

}
