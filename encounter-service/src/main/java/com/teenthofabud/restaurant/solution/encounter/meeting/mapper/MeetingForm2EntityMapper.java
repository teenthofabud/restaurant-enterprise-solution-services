package com.teenthofabud.restaurant.solution.encounter.meeting.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Slf4j
public abstract class MeetingForm2EntityMapper<T extends MeetingEntity, U extends MeetingForm>
        implements DualChannelMapper<T, U> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.encounter.meeting.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    protected Optional<T> compareAndMap(T actualEntity, T expectedEntity, U form) {
        boolean changeSW = false;

        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying MeetingEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying MeetingEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying MeetingEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());

        // comparative copy
        if(!fieldsToEscape.contains("sequence") && form.getSequence() != null && form.getSequence().compareTo(actualEntity.getSequence()) != 0) {
            expectedEntity.setSequence(form.getSequence());
            changeSW = true;
            log.debug("MeetingForm.sequence: {} is different as MeetingEntity.sequence: {}", form.getSequence(), actualEntity.getSequence());
        } else {
            expectedEntity.setSequence(actualEntity.getSequence());
            log.debug("MeetingForm.sequence: is unchanged");
        }
        if(!fieldsToEscape.contains("accountId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))
                && form.getAccountId().compareTo(actualEntity.getAccountId()) != 0) {
            expectedEntity.setAccountId(form.getAccountId());
            changeSW = true;
            log.debug("MeetingForm.accountId: {} is different as MeetingEntity.accountId: {}", form.getAccountId(), actualEntity.getAccountId());
        } else {
            expectedEntity.setAccountId(actualEntity.getAccountId());
            log.debug("MeetingForm.accountId: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}
