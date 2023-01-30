package com.teenthofabud.restaurant.solution.engagement.checkin.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Slf4j
public abstract class CheckInForm2EntityMapper<T extends CheckInEntity, U extends CheckInForm> implements DualChannelMapper<T, U> {


    public abstract List<String> getFieldsToEscape();

    protected Optional<T> compareAndMap(T actualEntity, T expectedEntity, U form) {
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying CheckInEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying CheckInEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying CheckInEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy

        if(!getFieldsToEscape().contains("sequence") && StringUtils.hasText(StringUtils.trimWhitespace(form.getSequence()))
                && form.getSequence().compareTo(actualEntity.getSequence()) != 0) {
            expectedEntity.setSequence(form.getSequence());
            changeSW = true;
            log.debug("CheckInForm.sequence: {} is different as CheckInEntity.sequence: {}", form.getSequence(), actualEntity.getSequence());
        } else {
            expectedEntity.setSequence(actualEntity.getSequence());
            log.debug("CheckInForm.sequence: is unchanged");
        }

        if(!getFieldsToEscape().contains("noOfPersons") && form.getNoOfPersons() != null && form.getNoOfPersons().compareTo(actualEntity.getNoOfPersons()) != 0) {
            expectedEntity.setNoOfPersons(form.getNoOfPersons());
            changeSW = true;
            log.debug("CheckInForm.noOfPersons: {} is different as CheckInEntity.noOfPersons: {}", form.getNoOfPersons(), actualEntity.getNoOfPersons());
        } else {
            expectedEntity.setNoOfPersons(actualEntity.getNoOfPersons());
            log.debug("CheckInForm.noOfPersons: is unchanged");
        }

        if(!getFieldsToEscape().contains("accountId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))
                && form.getAccountId().compareTo(actualEntity.getAccountId()) != 0) {
            expectedEntity.setAccountId(form.getAccountId());
            changeSW = true;
            log.debug("CheckInForm.accountId: {} is different as CheckInEntity.accountId: {}", form.getAccountId(), actualEntity.getAccountId());
        } else {
            expectedEntity.setAccountId(actualEntity.getAccountId());
            log.debug("CheckInForm.accountId: is unchanged");
        }

        if(!getFieldsToEscape().contains("notes") && StringUtils.hasText(StringUtils.trimWhitespace(form.getNotes()))
                && form.getNotes().compareTo(actualEntity.getNotes()) != 0) {
            expectedEntity.setNotes(form.getNotes());
            changeSW = true;
            log.debug("CheckInForm.notes: {} is different as CheckInEntity.notes: {}", form.getNotes(), actualEntity.getNotes());
        } else {
            expectedEntity.setNotes(actualEntity.getNotes());
            log.debug("CheckInForm.notes: is unchanged");
        }

        return changeSW ? Optional.of(expectedEntity) : Optional.empty();

    }

}
