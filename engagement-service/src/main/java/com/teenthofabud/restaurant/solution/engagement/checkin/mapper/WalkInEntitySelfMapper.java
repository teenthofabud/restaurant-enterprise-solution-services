package com.teenthofabud.restaurant.solution.engagement.checkin.mapper;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

@Component
@Slf4j
public class WalkInEntitySelfMapper extends CheckInEntitySelfMapper<WalkInEntity> {

    @Override
    public void compareAndMapChild(WalkInEntity source, WalkInEntity target) {
        boolean changeSW = false;

        super.compareAndMap(source, target);

        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source WalkInEntity.id is valid");
        }
        if(source.getName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getName())) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source WalkInEntity.name is valid");
        }
        if(source.getEmailId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getEmailId())) && source.getEmailId().compareTo(target.getEmailId()) != 0) {
            target.setEmailId(source.getEmailId());
            changeSW = true;
            log.debug("Source WalkInEntity.emailId is valid");
        }
        if(source.getPhoneNumber() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getPhoneNumber())) && source.getPhoneNumber().compareTo(target.getPhoneNumber()) != 0) {
            target.setPhoneNumber(source.getPhoneNumber());
            changeSW = true;
            log.debug("Source WalkInEntity.phoneNumber is valid");
        }

        if(changeSW) {
            log.debug("All provided WalkInEntity attributes are valid");
        } else {
            log.debug("Not all provided WalkInEntity attributes are valid");
        }
    }
}
