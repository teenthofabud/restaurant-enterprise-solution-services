package com.teenthofabud.restaurant.solution.engagement.checkin.mapper;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class ReservationEntitySelfMapper extends CheckInEntitySelfMapper<ReservationEntity> {

    @Override
    public void compareAndMapChild(ReservationEntity source, ReservationEntity target) {
        boolean changeSW = false;

        super.compareAndMap(source, target);

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
        } else {
            log.debug("Not all provided ReservationEntity attributes are valid");
        }
    }
}
