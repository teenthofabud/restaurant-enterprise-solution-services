package com.teenthofabud.restaurant.solution.engagement.checkin.repository;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationEntity;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

@Repository
public interface ReservationRepository extends CheckInRepository<ReservationEntity>, JpaSpecificationExecutor<ReservationEntity> {

}
