package com.teenthofabud.restaurant.solution.engagement.checkin.mapper;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class WalkInForm2EntityMapper extends CheckInForm2EntityMapper {

    private List<String> fieldsToEscape;

    @Value("#{'${res.engagement.checkIn.walkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<? extends CheckInEntity> compareAndMap(Optional<? extends CheckInEntity> optionalCheckInEntityChild, CheckInForm form) {
        WalkInEntity actualEntity = (WalkInEntity) optionalCheckInEntityChild.get();
        WalkInForm checkInFormParameters = (WalkInForm) form.getAttributes();
        boolean changeSW = false;
        // direct copy of common attributes handled in parent
        Optional<CheckInEntity> expectedParentEntity = super.compareAndMap(actualEntity, form);
        WalkInEntity expectedEntity = expectedParentEntity.isPresent() ? (WalkInEntity) expectedParentEntity.get() : new WalkInEntity();
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying WalkInEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());


        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(checkInFormParameters.getName())) && checkInFormParameters.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(checkInFormParameters.getName());
            changeSW = true;
            log.debug("WalkInForm.name: {} is different as WalkInEntity.name: {}", checkInFormParameters.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("WalkInForm.name: is unchanged");
        }

        if(!fieldsToEscape.contains("phoneNumber") && StringUtils.hasText(StringUtils.trimWhitespace(checkInFormParameters.getPhoneNumber()))
                && checkInFormParameters.getPhoneNumber().compareTo(actualEntity.getPhoneNumber()) != 0) {
            expectedEntity.setPhoneNumber(checkInFormParameters.getPhoneNumber());
            changeSW = true;
            log.debug("WalkInForm.phoneNumber: {} is different as WalkInEntity.phoneNumber: {}", checkInFormParameters.getPhoneNumber(), actualEntity.getPhoneNumber());
        } else {
            expectedEntity.setPhoneNumber(actualEntity.getPhoneNumber());
            log.debug("WalkInForm.phoneNumber: is unchanged");
        }

        if(!fieldsToEscape.contains("emailId") && StringUtils.hasText(StringUtils.trimWhitespace(checkInFormParameters.getEmailId())) && checkInFormParameters.getEmailId().compareTo(actualEntity.getEmailId()) != 0) {
            expectedEntity.setEmailId(checkInFormParameters.getEmailId());
            changeSW = true;
            log.debug("WalkInForm.emailId: {} is different as WalkInEntity.emailId: {}", checkInFormParameters.getEmailId(), actualEntity.getEmailId());
        } else {
            expectedEntity.setEmailId(actualEntity.getEmailId());
            log.debug("WalkInForm.emailId: is unchanged");
        }

        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
