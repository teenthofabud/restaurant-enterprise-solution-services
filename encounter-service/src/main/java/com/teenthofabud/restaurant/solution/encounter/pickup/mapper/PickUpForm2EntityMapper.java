package com.teenthofabud.restaurant.solution.encounter.pickup.mapper;

import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import com.teenthofabud.restaurant.solution.encounter.meeting.mapper.MeetingForm2EntityMapper;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpEntity;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class PickUpForm2EntityMapper extends MeetingForm2EntityMapper<PickUpEntity, PickUpForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.encounter.meeting.reservation.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected Optional<MeetingEntity> compareAndMap(MeetingEntity parent, PickUpEntity pickUpEntityChild, PickUpForm pickUpFormChild) {
        PickUpEntity actualEntity = pickUpEntityChild;
        PickUpForm form = pickUpFormChild;
        boolean changeSW = false;

        PickUpEntity expectedEntity = new PickUpEntity(parent);
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying PickUpEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());


        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName())) && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("PickUpForm.name: {} is different as PickUpEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("PickUpForm.name: is unchanged");
        }

        if(!fieldsToEscape.contains("phoneNumber") && StringUtils.hasText(StringUtils.trimWhitespace(form.getPhoneNo()))
                && form.getPhoneNo().compareTo(actualEntity.getPhoneNo()) != 0) {
            expectedEntity.setPhoneNo(form.getPhoneNo());
            changeSW = true;
            log.debug("PickUpForm.phoneNumber: {} is different as PickUpEntity.phoneNumber: {}", form.getPhoneNo(), actualEntity.getPhoneNo());
        } else {
            expectedEntity.setPhoneNo(actualEntity.getPhoneNo());
            log.debug("PickUpForm.phoneNumber: is unchanged");
        }

        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
