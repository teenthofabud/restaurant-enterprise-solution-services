package com.teenthofabud.restaurant.solution.engagement.checkin.repository;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationEntity;
import org.springframework.data.domain.Example;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface ReservationRepository extends CheckInRepository<ReservationEntity> {

    //public Optional<? extends CheckInEntity> findByName(String name);

}
