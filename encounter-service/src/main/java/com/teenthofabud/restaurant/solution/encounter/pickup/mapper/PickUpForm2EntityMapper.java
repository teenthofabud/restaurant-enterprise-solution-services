package com.teenthofabud.restaurant.solution.encounter.pickup.mapper;

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

    @Value("#{'${res.encounter.meeting.pickUp.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public List<String> getFieldsToEscape() {
        return this.fieldsToEscape;
    }

    @Override
    public Optional<PickUpEntity> compareAndMap(PickUpEntity pickUpEntity, PickUpForm pickUpForm) {
        boolean changeSW = false;
        // direct copy of common attributes handled in parent
        PickUpEntity expectedEntity = new PickUpEntity();
        // direct copy
        expectedEntity.setId(pickUpEntity.getId());
        log.debug("Directly copying PickUpEntity.id: {} from pickUpEntity to expectedEntity", pickUpEntity.getId());

        // parent copy
        Optional<PickUpEntity> optionalExpectedEntity = super.compareAndMap(pickUpEntity, expectedEntity, pickUpForm);
        if(optionalExpectedEntity.isPresent()) {
            expectedEntity = optionalExpectedEntity.get();
        }

        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(pickUpForm.getName())) && pickUpForm.getName().compareTo(pickUpEntity.getName()) != 0) {
            expectedEntity.setName(pickUpForm.getName());
            changeSW = true;
            log.debug("PickUpForm.name: {} is different as PickUpEntity.name: {}", pickUpForm.getName(), pickUpEntity.getName());
        } else {
            expectedEntity.setName(pickUpEntity.getName());
            log.debug("PickUpForm.name: is unchanged");
        }

        if(!fieldsToEscape.contains("phoneNumber") && StringUtils.hasText(StringUtils.trimWhitespace(pickUpForm.getPhoneNo()))
                && pickUpForm.getPhoneNo().compareTo(pickUpEntity.getPhoneNo()) != 0) {
            expectedEntity.setPhoneNo(pickUpForm.getPhoneNo());
            changeSW = true;
            log.debug("PickUpForm.phoneNumber: {} is different as PickUpEntity.phoneNumber: {}", pickUpForm.getPhoneNo(), pickUpEntity.getPhoneNo());
        } else {
            expectedEntity.setPhoneNo(pickUpEntity.getPhoneNo());
            log.debug("PickUpForm.phoneNumber: is unchanged");
        }

        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
