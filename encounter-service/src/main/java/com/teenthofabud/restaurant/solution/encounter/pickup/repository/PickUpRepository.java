package com.teenthofabud.restaurant.solution.encounter.pickup.repository;

import com.teenthofabud.restaurant.solution.encounter.meeting.repository.MeetingRepository;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpEntity;
import org.springframework.stereotype.Repository;

@Repository
public interface PickUpRepository extends MeetingRepository<PickUpEntity> {

}
