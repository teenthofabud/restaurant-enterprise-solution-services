package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import lombok.*;
import org.apache.commons.lang3.SerializationUtils;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;
import java.time.LocalDate;
import java.time.LocalTime;

@Getter
@Setter
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true, callSuper = true)
@Entity(name = "reservation")
@Table(name = "reservation")
@EntityListeners(AuditingEntityListener.class)
@PrimaryKeyJoinColumn(name = "engagement_check_in_id")
@NoArgsConstructor
public class ReservationEntity extends CheckInEntity {

    public ReservationEntity(CheckInEntity parent) {
        super(parent);
        this.date = LocalDate.now();
        this.time = LocalTime.now();
    }

    @ToString.Include
    private LocalDate date;
    @ToString.Include
    private LocalTime time;

}
