package com.teenthofabud.restaurant.solution.engagement.checkin.mapper;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class WalkInForm2EntityMapper extends CheckInForm2EntityMapper<WalkInEntity, WalkInForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.engagement.checkIn.walkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    /*@Override
    public Optional<WalkInEntity> compareAndMap(CheckInEntity parent, WalkInEntity checkInEntityChild, WalkInForm checkInFormChild) {
        WalkInEntity actualEntity = (WalkInEntity) checkInEntityChild;
        WalkInForm form = checkInFormChild;
        boolean changeSW = false;
        // direct copy of common attributes handled in parent
        WalkInEntity expectedEntity = new WalkInEntity(parent);
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying WalkInEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());


        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName())) && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("WalkInForm.name: {} is different as WalkInEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("WalkInForm.name: is unchanged");
        }

        if(!fieldsToEscape.contains("phoneNumber") && StringUtils.hasText(StringUtils.trimWhitespace(form.getPhoneNumber()))
                && form.getPhoneNumber().compareTo(actualEntity.getPhoneNumber()) != 0) {
            expectedEntity.setPhoneNumber(form.getPhoneNumber());
            changeSW = true;
            log.debug("WalkInForm.phoneNumber: {} is different as WalkInEntity.phoneNumber: {}", form.getPhoneNumber(), actualEntity.getPhoneNumber());
        } else {
            expectedEntity.setPhoneNumber(actualEntity.getPhoneNumber());
            log.debug("WalkInForm.phoneNumber: is unchanged");
        }

        if(!fieldsToEscape.contains("emailId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getEmailId())) && form.getEmailId().compareTo(actualEntity.getEmailId()) != 0) {
            expectedEntity.setEmailId(form.getEmailId());
            changeSW = true;
            log.debug("WalkInForm.emailId: {} is different as WalkInEntity.emailId: {}", form.getEmailId(), actualEntity.getEmailId());
        } else {
            expectedEntity.setEmailId(actualEntity.getEmailId());
            log.debug("WalkInForm.emailId: is unchanged");
        }

        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }*/

    @Override
    public Optional<WalkInEntity> compareAndMap(WalkInEntity actualEntity, WalkInForm form) {
        boolean changeSW = false;
        // direct copy of common attributes handled in parent
        WalkInEntity expectedEntity = new WalkInEntity();
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying WalkInEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());

        // parent copy
        Optional<WalkInEntity> optionalExpectedEntity = super.compareAndMap(actualEntity, expectedEntity, form);
        if(optionalExpectedEntity.isPresent()) {
            expectedEntity = optionalExpectedEntity.get();
        }

        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName())) && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("WalkInForm.name: {} is different as WalkInEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("WalkInForm.name: is unchanged");
        }

        if(!fieldsToEscape.contains("phoneNumber") && StringUtils.hasText(StringUtils.trimWhitespace(form.getPhoneNumber()))
                && form.getPhoneNumber().compareTo(actualEntity.getPhoneNumber()) != 0) {
            expectedEntity.setPhoneNumber(form.getPhoneNumber());
            changeSW = true;
            log.debug("WalkInForm.phoneNumber: {} is different as WalkInEntity.phoneNumber: {}", form.getPhoneNumber(), actualEntity.getPhoneNumber());
        } else {
            expectedEntity.setPhoneNumber(actualEntity.getPhoneNumber());
            log.debug("WalkInForm.phoneNumber: is unchanged");
        }

        if(!fieldsToEscape.contains("emailId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getEmailId())) && form.getEmailId().compareTo(actualEntity.getEmailId()) != 0) {
            expectedEntity.setEmailId(form.getEmailId());
            changeSW = true;
            log.debug("WalkInForm.emailId: {} is different as WalkInEntity.emailId: {}", form.getEmailId(), actualEntity.getEmailId());
        } else {
            expectedEntity.setEmailId(actualEntity.getEmailId());
            log.debug("WalkInForm.emailId: is unchanged");
        }

        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}
