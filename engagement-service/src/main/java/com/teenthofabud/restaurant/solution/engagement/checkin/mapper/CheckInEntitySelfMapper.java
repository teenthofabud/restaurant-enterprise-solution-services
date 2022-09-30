package com.teenthofabud.restaurant.solution.engagement.checkin.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Slf4j
public abstract class CheckInEntitySelfMapper implements SingleChannelMapper<CheckInEntity> {

    @Override
    public Optional<CheckInEntity> compareAndMap(CheckInEntity source, CheckInEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source CheckInEntity.id is valid");
        }
        /*if(source.getTableId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getTableId())) && source.getTableId().compareTo(target.getTableId()) != 0) {
            target.setTableId(source.getTableId());
            changeSW = true;
            log.debug("Source CheckInEntity.tableId is valid");
        }*/
        if(source.getNoOfPersons() != null && source.getNoOfPersons().compareTo(target.getNoOfPersons()) != 0) {
            target.setNoOfPersons(source.getNoOfPersons());
            changeSW = true;
            log.debug("Source CheckInEntity.noOfPersons is valid");
        }
        if(source.getSequence() != null && source.getSequence().compareTo(target.getSequence()) != 0) {
            target.setSequence(source.getSequence());
            changeSW = true;
            log.debug("Source CheckInEntity.sequence is valid");
        }
        if(source.getAccountId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getAccountId())) && source.getAccountId().compareTo(target.getAccountId()) != 0) {
            target.setAccountId(source.getAccountId());
            changeSW = true;
            log.debug("Source CheckInEntity.accountId is valid");
        }
        if(source.getNotes() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getNotes())) && source.getNotes().compareTo(target.getNotes()) != 0) {
            target.setNotes(source.getNotes());
            changeSW = true;
            log.debug("Source CheckInEntity.notes is valid");
        }
        /*
        if(source.getStatusHistory() != null && !CollectionUtils.isEmpty(source.getStatusHistory()) && !source.getStatusHistory().containsAll(target.getStatusHistory())) {
            target.setStatusHistory(source.getStatusHistory());
            changeSW = true;
            log.debug("Source CheckInEntity.statusHistory is valid");
        }*/
        if(changeSW) {
            log.debug("All provided CheckInEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided CheckInEntity attributes are valid");
            return Optional.empty();
        }
    }
    public abstract Optional<? extends CheckInEntity> compareAndMap(Optional<? extends CheckInEntity> optionalSource, Optional<? extends CheckInEntity> optionalTarget);

}
