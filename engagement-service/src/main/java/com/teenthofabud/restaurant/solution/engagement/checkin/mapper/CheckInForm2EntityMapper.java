package com.teenthofabud.restaurant.solution.engagement.checkin.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInHistoryDocument;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInStatus;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class CheckInForm2EntityMapper implements DualChannelMapper<CheckInEntity, CheckInForm> {

    private List<String> fieldsToEscape;
    private String checkInTimeFormat;

    @Value("#{'${res.engagement.checkin.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Value("${res.engagement.checkin.timestamp}")
    public void setCheckInTimeFormat(String checkInTimeFormat) {
        this.checkInTimeFormat = checkInTimeFormat;
    }

    @Override
    public Optional<CheckInEntity> compareAndMap(CheckInEntity actualDocument, CheckInForm form) {
        CheckInEntity expectedDocument = new CheckInEntity();
        boolean changeSW = false;
        // direct copy
        expectedDocument.setId(actualDocument.getId());
        log.debug("Directly copying CheckInEntity.id: {} from actualDocument to expectedDocument", actualDocument.getId());
        expectedDocument.setCreatedOn(actualDocument.getCreatedOn());
        log.debug("Directly copying CheckInEntity.createdOn: {} from actualDocument to expectedDocument", actualDocument.getCreatedOn());
        expectedDocument.setActive(actualDocument.getActive());
        log.debug("Directly copying CheckInEntity.active: {} from actualDocument to expectedDocument", actualDocument.getActive());
        // comparative copy

        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName())) && form.getName().compareTo(actualDocument.getName()) != 0) {
            expectedDocument.setName(form.getName());
            changeSW = true;
            log.debug("CheckInForm.name: {} is different as CheckInEntity.name: {}", form.getName(), actualDocument.getName());
        } else {
            expectedDocument.setName(actualDocument.getName());
            log.debug("CheckInForm.name: is unchanged");
        }

        if(!fieldsToEscape.contains("phoneNumber") && StringUtils.hasText(StringUtils.trimWhitespace(form.getPhoneNumber()))
                && form.getPhoneNumber().compareTo(actualDocument.getPhoneNumber()) != 0) {
            expectedDocument.setPhoneNumber(form.getPhoneNumber());
            changeSW = true;
            log.debug("CheckInForm.phoneNumber: {} is different as CheckInEntity.phoneNumber: {}", form.getPhoneNumber(), actualDocument.getPhoneNumber());
        } else {
            expectedDocument.setPhoneNumber(actualDocument.getPhoneNumber());
            log.debug("CheckInForm.phoneNumber: is unchanged");
        }

        if(!fieldsToEscape.contains("emailId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getEmailId())) && form.getEmailId().compareTo(actualDocument.getEmailId()) != 0) {
            expectedDocument.setEmailId(form.getEmailId());
            changeSW = true;
            log.debug("CheckInForm.emailId: {} is different as CheckInEntity.emailId: {}", form.getEmailId(), actualDocument.getEmailId());
        } else {
            expectedDocument.setEmailId(actualDocument.getEmailId());
            log.debug("CheckInForm.emailId: is unchanged");
        }

        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName())) && form.getName().compareTo(actualDocument.getName()) != 0) {
            expectedDocument.setName(form.getName());
            changeSW = true;
            log.debug("CheckInForm.name: {} is different as CheckInEntity.name: {}", form.getName(), actualDocument.getName());
        } else {
            expectedDocument.setName(actualDocument.getName());
            log.debug("CheckInForm.name: is unchanged");
        }

        if(!fieldsToEscape.contains("status") && StringUtils.hasText(StringUtils.trimWhitespace(form.getStatus()))) {
            CheckInStatus expectedStatus = CheckInStatus.valueOf(form.getStatus());
            CheckInHistoryDocument checkInHistoryDocument = actualDocument.getStatusHistory().get(actualDocument.getStatusHistory().size() - 1);
            if(expectedStatus.compareTo(checkInHistoryDocument.getStatus()) != 0) {
                expectedDocument.addStatus(expectedStatus);
                changeSW = true;
                log.debug("CheckInForm.status: {} is different as CheckInEntity.status: {}", form.getStatus(), checkInHistoryDocument.getStatus());
            } else {
                expectedDocument.setStatusHistory(actualDocument.getStatusHistory());
                log.debug("CheckInForm.status: is unchanged");
            }
        } else {
            expectedDocument.setStatusHistory(actualDocument.getStatusHistory());
            log.debug("CheckInForm.status: is unchanged");
        }

        if(!fieldsToEscape.contains("sequence") && form.getSequence() != null && form.getSequence().compareTo(actualDocument.getSequence()) != 0) {
            expectedDocument.setSequence(form.getSequence());
            changeSW = true;
            log.debug("CheckInForm.sequence: {} is different as CheckInEntity.sequence: {}", form.getSequence(), actualDocument.getSequence());
        } else {
            expectedDocument.setSequence(actualDocument.getSequence());
            log.debug("CheckInForm.sequence: is unchanged");
        }

        if(!fieldsToEscape.contains("noOfPersons") && form.getNoOfPersons() != null && form.getNoOfPersons().compareTo(actualDocument.getNoOfPersons()) != 0) {
            expectedDocument.setNoOfPersons(form.getNoOfPersons());
            changeSW = true;
            log.debug("CheckInForm.noOfPersons: {} is different as CheckInEntity.noOfPersons: {}", form.getNoOfPersons(), actualDocument.getNoOfPersons());
        } else {
            expectedDocument.setNoOfPersons(actualDocument.getNoOfPersons());
            log.debug("CheckInForm.noOfPersons: is unchanged");
        }

        if(!fieldsToEscape.contains("accountId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))
                && form.getAccountId().compareTo(actualDocument.getAccountId()) != 0) {
            expectedDocument.setAccountId(form.getAccountId());
            changeSW = true;
            log.debug("CheckInForm.accountId: {} is different as CheckInEntity.accountId: {}", form.getAccountId(), actualDocument.getAccountId());
        } else {
            expectedDocument.setAccountId(actualDocument.getAccountId());
            log.debug("CheckInForm.accountId: is unchanged");
        }

        return changeSW ? Optional.of(expectedDocument) : Optional.empty();
    }

}
