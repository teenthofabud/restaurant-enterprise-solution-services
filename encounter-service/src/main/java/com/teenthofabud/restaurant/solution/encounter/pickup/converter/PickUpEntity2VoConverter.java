package com.teenthofabud.restaurant.solution.encounter.pickup.converter;

import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingVo;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpEntity;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.List;

@Slf4j
@Component
public class PickUpEntity2VoConverter extends MeetingEntity2VoConverter<PickUpEntity, PickUpVo> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.encounter.meeting.pickUp.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected PickUpVo convertChild(PickUpEntity entity, MeetingVo meetingVo) {
        PickUpVo vo = new PickUpVo(meetingVo);
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("phoneNo")) {
            vo.setPhoneNo(entity.getPhoneNo());
        }
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

}
