package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Slf4j
@Component
public class WalkInEntity2VoConverter extends CheckInEntity2VoConverter<WalkInEntity, WalkInVo> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.engagement.checkIn.walkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected WalkInVo convertChild(WalkInEntity entity, CheckInVo checkInVo) {
        WalkInVo vo = new WalkInVo(checkInVo);
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

    @Override
    public List<String> getFieldsToEscape() {
        return this.fieldsToEscape;
    }

}
