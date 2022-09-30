package com.teenthofabud.restaurant.solution.engagement.checkin.mapper;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class ReservationEntitySelfMapper extends CheckInEntitySelfMapper {

    @Override
    public Optional<? extends CheckInEntity> compareAndMap(Optional<? extends CheckInEntity> optionalSource, Optional<? extends CheckInEntity> optionalTarget) {
        CheckInEntity checkInEntitySource = optionalSource.get();
        CheckInEntity checkInEntityTarget = optionalTarget.get();
        Optional<CheckInEntity> optionalCheckInEntity = super.compareAndMap(checkInEntitySource, checkInEntityTarget);
        if(optionalCheckInEntity.isEmpty()) {
            return Optional.empty();
        }

        boolean changeSW = false;
        ReservationEntity source = (ReservationEntity) checkInEntitySource;
        ReservationEntity target = (ReservationEntity) checkInEntityTarget;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source ReservationEntity.id is valid");
        }
        if(source.getDate() != null && source.getDate().compareTo(target.getDate()) != 0) {
            target.setDate(source.getDate());
            changeSW = true;
            log.debug("Source ReservationEntity.date is valid");
        }
        if(source.getTime() != null && source.getTime().compareTo(target.getTime()) != 0) {
            target.setTime(source.getTime());
            changeSW = true;
            log.debug("Source ReservationEntity.time is valid");
        }

        if(changeSW) {
            log.debug("All provided ReservationEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided ReservationEntity attributes are valid");
            return Optional.empty();
        }
    }
}
