package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.Table;
import java.time.LocalDate;
import java.time.LocalTime;

@Getter
@Setter
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "reservation")
@EntityListeners(AuditingEntityListener.class)
public class ReservationEntity extends CheckInEntity {

    public ReservationEntity() {
        this.date = LocalDate.now();
        this.time = LocalTime.now();
    }

    private LocalDate date;
    private LocalTime time;

}
