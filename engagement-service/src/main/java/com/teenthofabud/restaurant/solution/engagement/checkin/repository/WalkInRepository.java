package com.teenthofabud.restaurant.solution.engagement.checkin.repository;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInEntity;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface WalkInRepository extends CheckInRepository<WalkInEntity> {

}
