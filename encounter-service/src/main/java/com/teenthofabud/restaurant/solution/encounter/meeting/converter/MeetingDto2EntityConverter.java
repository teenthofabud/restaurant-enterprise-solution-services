package com.teenthofabud.restaurant.solution.encounter.meeting.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingDto;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Slf4j
public abstract class MeetingDto2EntityConverter<T extends MeetingDto, U extends MeetingEntity>
        implements ComparativePatchConverter<MeetingDto, MeetingEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 3;

    public abstract List<String> getFieldsToEscape();

    @Override
    public void compareAndMap(MeetingDto dto, MeetingEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;

        Optional<String> optSequence = dto.getSequence();
        if(!getFieldsToEscape().contains("sequence") && optSequence.isPresent()) {
            actualEntity.setSequence(optSequence.get());
            changeSW[i++] = true;
            log.debug("MeetingDto.sequence is valid");
        }

        Optional<String> optAccountId = dto.getAccountId();
        if(!getFieldsToEscape().contains("accountId") && optAccountId.isPresent()) {
            actualEntity.setAccountId(optAccountId.get());
            changeSW[i++] = true;
            log.debug("MeetingDto.accountId is valid");
        }

        Optional<String> optActive = dto.getActive();
        if(!getFieldsToEscape().contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("MeetingDto.active is valid");
        }

        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided MeetingDto attributes are valid");
            //this.compareAndMap(dto, actualEntity);
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided MeetingDto attributes are valid");
    }

    public abstract void compareAndMapChild(T checkInDtoChild, U checkInEntityChild) throws TOABBaseException;
}
