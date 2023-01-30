package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInDto;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Slf4j
public abstract class CheckInDto2EntityConverter<T extends CheckInDto, U extends CheckInEntity> implements ComparativePatchConverter<CheckInDto, CheckInEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 6;

    public abstract List<String> getFieldsToEscape();
    
    @Override
    public void compareAndMap(CheckInDto dto, CheckInEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;

        /*

        Optional<String> optStatus = dto.getStatus();
        if(!getFieldsToEscape().contains("status") && optStatus.isPresent()) {
            CheckInStatus status = CheckInStatus.valueOf(optStatus.get());
            actualEntity.addStatus(status);
            changeSW[i++] = true;
            log.debug("CheckInDto.status is valid");
        }*/

        Optional<Integer> optNoOfPersons = dto.getNoOfPersons();
        if(!getFieldsToEscape().contains("noOfPersons") && optNoOfPersons.isPresent()) {
            actualEntity.setNoOfPersons(optNoOfPersons.get());
            changeSW[i++] = true;
            log.debug("CheckInDto.noOfPersons is valid");
        }

        Optional<String> optAccountId = dto.getAccountId();
        if(!getFieldsToEscape().contains("accountId") && optAccountId.isPresent()) {
            actualEntity.setAccountId(optAccountId.get());
            changeSW[i++] = true;
            log.debug("CheckInDto.accountId is valid");
        }

        Optional<String> optNotes = dto.getNotes();
        if(!getFieldsToEscape().contains("notes") && optNotes.isPresent()) {
            actualEntity.setNotes(optNotes.get());
            changeSW[i++] = true;
            log.debug("CheckInDto.notes is valid");
        }

        Optional<String> optSequence = dto.getSequence();
        if(!getFieldsToEscape().contains("sequence") && optSequence.isPresent()) {
            actualEntity.setSequence(optSequence.get());
            changeSW[i++] = true;
            log.debug("CheckInDto.sequence is valid");
        }

        Optional<String> optActive = dto.getActive();
        if(!getFieldsToEscape().contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("CheckInDto.active is valid");
        }

        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided CheckInDto attributes are valid");
            //this.compareAndMap(dto, actualEntity);
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided CheckInDto attributes are valid");
    }

    public abstract void compareAndMapChild(T checkInDtoChild, U checkInEntityChild) throws TOABBaseException;
}
