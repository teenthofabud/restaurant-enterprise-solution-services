package com.teenthofabud.restaurant.solution.encounter.meeting.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Slf4j
public abstract class MeetingEntitySelfMapper<T extends MeetingEntity> implements SingleChannelMapper<MeetingEntity> {

    @Override
    public Optional<MeetingEntity> compareAndMap(MeetingEntity source, MeetingEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source MeetingEntity.id is valid");
        }
        if(source.getSequence() != null && source.getSequence().compareTo(target.getSequence()) != 0) {
            target.setSequence(source.getSequence());
            changeSW = true;
            log.debug("Source MeetingEntity.sequence is valid");
        }
        if(source.getAccountId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getAccountId())) && source.getAccountId().compareTo(target.getAccountId()) != 0) {
            target.setAccountId(source.getAccountId());
            changeSW = true;
            log.debug("Source MeetingEntity.accountId is valid");
        }
        if(changeSW) {
            log.debug("All provided MeetingEntity attributes are valid");
            this.compareAndMapChild((T) source, (T) target);
            return Optional.of(target);
        } else {
            log.debug("Not all provided MeetingEntity attributes are valid");
            return Optional.empty();
        }
    }
    protected abstract void compareAndMapChild(T source, T target);

}
