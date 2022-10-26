package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.Table;


@Getter
@Setter
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "walk_in")
@EntityListeners(AuditingEntityListener.class)
@NoArgsConstructor
public class WalkInEntity extends CheckInEntity {

    public WalkInEntity(CheckInEntity parent) {
        super(parent);
        this.name = "";
        this.phoneNumber = "";
        this.emailId = "";
    }

    private String name;
    @Column(name = "phone_number")
    private String phoneNumber;
    @Column(name = "email_id")
    private String emailId;

}
