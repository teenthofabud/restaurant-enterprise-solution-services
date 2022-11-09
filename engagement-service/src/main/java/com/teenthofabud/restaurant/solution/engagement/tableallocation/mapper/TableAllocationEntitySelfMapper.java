package com.teenthofabud.restaurant.solution.engagement.tableallocation.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class TableAllocationEntitySelfMapper implements SingleChannelMapper<TableAllocationEntity> {

    @Override
    public Optional<TableAllocationEntity> compareAndMap(TableAllocationEntity source, TableAllocationEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source TableAllocationEntity.id is valid");
        }
        if(source.getTableId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getTableId())) && source.getTableId().compareTo(target.getTableId()) != 0) {
            target.setTableId(source.getTableId());
            changeSW = true;
            log.debug("Source TableAllocationEntity.tableId is valid");
        }
        if(source.getNotes() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getNotes())) && source.getNotes().compareTo(target.getNotes()) != 0) {
            target.setNotes(source.getNotes());
            changeSW = true;
            log.debug("Source TableAllocationEntity.notes is valid");
        }
        /*
        if(source.getStatusHistory() != null && !CollectionUtils.isEmpty(source.getStatusHistory()) && !source.getStatusHistory().containsAll(target.getStatusHistory())) {
            target.setStatusHistory(source.getStatusHistory());
            changeSW = true;
            log.debug("Source TableAllocationEntity.statusHistory is valid");
        }*/
        if(changeSW) {
            log.debug("All provided TableAllocationEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided TableAllocationEntity attributes are valid");
            return Optional.empty();
        }
    }
}
