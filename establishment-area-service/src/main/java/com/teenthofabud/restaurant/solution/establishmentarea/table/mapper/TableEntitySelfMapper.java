package com.teenthofabud.restaurant.solution.establishmentarea.table.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class TableEntitySelfMapper implements SingleChannelMapper<TableEntity> {

    @Override
    public Optional<TableEntity> compareAndMap(TableEntity source, TableEntity target) {
        boolean changeSW = false;
        if(source.getTableId() != null && source.getTableId().compareTo(target.getTableId()) != 0) {
            target.setTableId(source.getTableId());
            changeSW = true;
            log.debug("Source TableEntity.tableId is valid");
        }
        if(source.getTableName() != null &&
                StringUtils.hasText(StringUtils.trimWhitespace(source.getTableName()))
                && source.getTableName().compareTo(target.getTableName()) != 0) {
            target.setTableName(source.getTableName());
            changeSW = true;
            log.debug("Source TableEntity.tableName is valid");
        }
        if(source.getDescription() != null &&
                StringUtils.hasText(StringUtils.trimWhitespace(source.getDescription()))
                && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source TableEntity.description is valid");
        }
        if(source.getCapacity() != null &&
                StringUtils.hasText(StringUtils.trimWhitespace(source.getCapacity()))
                && source.getCapacity().compareTo(target.getCapacity()) != 0) {
            target.setCapacity(source.getCapacity());
            changeSW = true;
            log.debug("Source TableEntity.capacity is valid");
        }
        if(source.getFloor() != null && source.getFloor().compareTo(target.getFloor()) != 0) {
            target.setFloor(source.getFloor());
            changeSW = true;
            log.debug("Source TableEntity.floor is valid");
        }
        if(changeSW) {
            log.debug("All provided TableEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided TableEntity attributes are valid");
            return Optional.empty();
        }
    }
}
