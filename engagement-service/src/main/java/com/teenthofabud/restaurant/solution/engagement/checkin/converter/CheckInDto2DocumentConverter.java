package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInDocument;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInDto;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInStatus;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class CheckInDto2DocumentConverter implements ComparativePatchConverter<CheckInDto, CheckInDocument> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 10;

    private List<String> fieldsToEscape;

    @Value("#{'${res.engagement.checkin.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public void compareAndMap(CheckInDto dto, CheckInDocument actualDocument) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;

        Optional<String> optTableId = dto.getTableId();
        if(!fieldsToEscape.contains("tableId") && optTableId.isPresent()) {
            actualDocument.setTableId(optTableId.get());
            changeSW[i++] = true;
            log.debug("CheckInDto.tableId is valid");
        }

        Optional<String> optStatus = dto.getStatus();
        if(!fieldsToEscape.contains("status") && optStatus.isPresent()) {
            CheckInStatus status = CheckInStatus.valueOf(optStatus.get());
            actualDocument.addStatus(status);
            changeSW[i++] = true;
            log.debug("CheckInDto.status is valid");
        }

        Optional<Integer> optNoOfPersons = dto.getNoOfPersons();
        if(!fieldsToEscape.contains("noOfPersons") && optNoOfPersons.isPresent()) {
            actualDocument.setNoOfPersons(optNoOfPersons.get());
            changeSW[i++] = true;
            log.debug("CheckInDto.noOfPersons is valid");
        }

        Optional<Long> optSequence = dto.getSequence();
        if(!fieldsToEscape.contains("sequence") && optSequence.isPresent()) {
            actualDocument.setSequence(optSequence.get());
            changeSW[i++] = true;
            log.debug("CheckInDto.sequence is valid");
        }

        Optional<String> optAccountId = dto.getAccountId();
        if(!fieldsToEscape.contains("accountId") && optAccountId.isPresent()) {
            actualDocument.setAccountId(optAccountId.get());
            changeSW[i++] = true;
            log.debug("CheckInDto.accountId is valid");
        }

        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent()) {
            actualDocument.setName(optName.get());
            changeSW[i++] = true;
            log.debug("CheckInDto.name is valid");
        }

        Optional<String> optEmailId = dto.getEmailId();
        if(!fieldsToEscape.contains("emailId") && optEmailId.isPresent()) {
            actualDocument.setEmailId(optEmailId.get());
            changeSW[i++] = true;
            log.debug("CheckInDto.emailId is valid");
        }

        Optional<String> optPhoneNumber = dto.getPhoneNumber();
        if(!fieldsToEscape.contains("phoneNumber") && optPhoneNumber.isPresent()) {
            actualDocument.setPhoneNumber(optPhoneNumber.get());
            changeSW[i++] = true;
            log.debug("CheckInDto.phoneNumber is valid");
        }

        Optional<String> optNotes = dto.getNotes();
        if(!fieldsToEscape.contains("notes") && optNotes.isPresent()) {
            actualDocument.setNotes(optNotes.get());
            changeSW[i++] = true;
            log.debug("CheckInDto.notes is valid");
        }

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualDocument.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("CheckInDto.active is valid");
        }

        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided CheckInDto attributes are valid");
            actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided CheckInDto attributes are valid");
    }

}
