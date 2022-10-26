package com.teenthofabud.restaurant.solution.engagement.checkin.repository;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInEntity;
import org.springframework.stereotype.Repository;

@Repository
public interface WalkInRepository extends CheckInRepository<WalkInEntity> {

    //public Optional<? extends CheckInEntity> findByDateAndTime(LocalDate date, LocalTime time);

}
