package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Slf4j
@Component
public class WalkInEntity2VoConverter extends CheckInEntity2VoConverter {

    private List<String> fieldsToEscape;

    @Value("#{'${res.engagement.checkIn.walkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected CheckInVoParameters convert(Optional<? extends CheckInEntity> optionalCheckInEntityChild) {
        WalkInEntity entity = (WalkInEntity) optionalCheckInEntityChild.get();
        WalkInVo vo = new WalkInVo();
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("emailId")) {
            vo.setEmailId(entity.getEmailId());
        }
        if(!fieldsToEscape.contains("phoneNumber")) {
            vo.setPhoneNumber(entity.getPhoneNumber());
        }
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

}
