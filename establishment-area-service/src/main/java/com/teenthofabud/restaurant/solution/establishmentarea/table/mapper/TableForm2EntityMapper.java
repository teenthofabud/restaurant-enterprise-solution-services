package com.teenthofabud.restaurant.solution.establishmentarea.table.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.repository.FloorRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class TableForm2EntityMapper implements DualChannelMapper<TableEntity, TableForm> {

    private List<String> fieldsToEscape;
    private FloorRepository floorRepository;

    @Value("#{'${res.establishment.area.table.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setFloorRepository(FloorRepository floorRepository) {
        this.floorRepository = floorRepository;
    }

    @Override
    public Optional<TableEntity> compareAndMap(TableEntity actualEntity, TableForm form) {
        TableEntity expectedEntity = new TableEntity();
        boolean changeSW = false;
        // direct copy
        log.debug("Directly copying TableEntity.tableId: {} from actualEntity to expectedEntity", actualEntity.getTableId());
        expectedEntity.setTableId(actualEntity.getTableId());
        log.debug("Directly copying TableEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying TableEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        expectedEntity.setActive(actualEntity.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("tableName") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTableName()))
                && form.getTableName().compareTo(actualEntity.getTableName()) != 0) {
            expectedEntity.setTableName(form.getTableName());
            changeSW = true;
            log.debug("TableForm.tableName: {} is different as TableEntity.tableName: {}", form.getTableName(), actualEntity.getTableName());
        } else {
            expectedEntity.setTableName(actualEntity.getTableName());
            log.debug("TableForm.tableName: is unchanged");
        }
        
        if(!fieldsToEscape.contains("description") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription()))
                && form.getDescription().compareTo(actualEntity.getDescription()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("TableForm.description: {} is different as TableEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            log.debug("TableForm.description: is unchanged");
        }
        
        if(!fieldsToEscape.contains("capacity") && form.getCapacity() != null && form.getCapacity().compareTo(actualEntity.getCapacity()) != 0) {
            expectedEntity.setCapacity(form.getCapacity());
            changeSW = true;
            log.debug("TableForm.capacity: {} is different as TableEntity.capacity: {}", form.getCapacity(), actualEntity.getCapacity());
        } else {
            expectedEntity.setCapacity(actualEntity.getCapacity());
            log.debug("TableForm.capacity: is unchanged");
        }
        
        if(!fieldsToEscape.contains("floorId") && form.getFloorId() != null) {
            Long floorId = Long.parseLong(form.getFloorId());
            Optional<FloorEntity> optFloorEntity = floorRepository.findById(floorId);
            if(actualEntity.getFloor().compareTo(optFloorEntity.get()) != 0) {
                expectedEntity.setFloor(optFloorEntity.get());
                changeSW = true;
                log.debug("TableForm.floorId: {} is different as FloorEntity.floorId: {}", form.getFloorId(), actualEntity.getFloor().getFlrId());
            } else {
                expectedEntity.setFloor(actualEntity.getFloor());
                log.debug("FloorForm.floorId: is unchanged");
            }
        } else {
            expectedEntity.setFloor(actualEntity.getFloor());
            log.debug("FloorForm.floorId: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
