package com.teenthofabud.restaurant.solution.encounter.delivery.converter;

import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryEntity;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingForm2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class DeliveryForm2EntityConverter extends MeetingForm2EntityConverter<DeliveryForm, DeliveryEntity> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.encounter.meeting.delivery.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected DeliveryEntity convertChild(DeliveryForm form, MeetingEntity meetingEntity) {
        DeliveryEntity entity = new DeliveryEntity(meetingEntity);
        if(!fieldsToEscape.contains("orderId")) {
            entity.setOrderId(form.getOrderId());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
