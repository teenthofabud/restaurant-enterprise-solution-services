package com.teenthofabud.restaurant.solution.encounter.delivery.converter;

import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryEntity;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryVo;
import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.List;

@Slf4j
@Component
public class DeliveryEntity2VoConverter extends MeetingEntity2VoConverter<DeliveryEntity, DeliveryVo> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.encounter.meeting.delivery.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected DeliveryVo convertChild(DeliveryEntity entity, MeetingVo meetingVo) {
        DeliveryVo vo = new DeliveryVo(meetingVo);
        if(!fieldsToEscape.contains("orderId")) {
            vo.setOrderId(entity.getOrderId());
        }
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

}
