package com.teenthofabud.restaurant.solution.engagement.checkin.repository;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInEntity;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.Optional;

@Repository
public interface ReservationRepository extends CheckInRepository<ReservationEntity> {

    public Optional<? extends CheckInEntity> findByName(String name);

}
