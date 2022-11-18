package com.teenthofabud.restaurant.solution.encounter.pickup.mapper;

import com.teenthofabud.restaurant.solution.encounter.meeting.mapper.MeetingEntitySelfMapper;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

@Component
@Slf4j
public class PickUpEntitySelfMapper extends MeetingEntitySelfMapper<PickUpEntity> {

    @Override
    protected void compareAndMapChild(PickUpEntity source, PickUpEntity target) {
        boolean changeSW = false;

        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source PickUpEntity.id is valid");
        }
        if(source.getName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getName())) &&
                source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source DeliveryEntity.name is valid");
        }
        if(source.getPhoneNo() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getPhoneNo())) &&
                source.getPhoneNo().compareTo(target.getPhoneNo()) != 0) {
            target.setPhoneNo(source.getPhoneNo());
            changeSW = true;
            log.debug("Source DeliveryEntity.phoneNumber is valid");
        }

        if(changeSW) {
            log.debug("All provided PickUpEntity attributes are valid");
        } else {
            log.debug("Not all provided PickUpEntity attributes are valid");
        }
    }
}
