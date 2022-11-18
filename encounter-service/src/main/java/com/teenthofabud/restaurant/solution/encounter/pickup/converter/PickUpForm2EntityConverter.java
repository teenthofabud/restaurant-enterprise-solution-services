package com.teenthofabud.restaurant.solution.encounter.pickup.converter;

import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingForm2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpEntity;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class PickUpForm2EntityConverter extends MeetingForm2EntityConverter<PickUpForm, PickUpEntity> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.encounter.meeting.pickUp.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected PickUpEntity convertChild(PickUpForm form, MeetingEntity meetingEntity) {
        PickUpEntity entity = new PickUpEntity(meetingEntity);
        if(!fieldsToEscape.contains("name")) {
            entity.setName(form.getName());
        }
        if(!fieldsToEscape.contains("phoneNo")) {
            entity.setPhoneNo(form.getPhoneNo());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
