package com.teenthofabud.restaurant.solution.engagement.tableallocation.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationEntity;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class TableAllocationForm2EntityMapper implements DualChannelMapper<TableAllocationEntity, TableAllocationForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.engagement.tableAllocation.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<TableAllocationEntity> compareAndMap(TableAllocationEntity actualEntity, TableAllocationForm form) {
        TableAllocationEntity expectedEntity = new TableAllocationEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying TableAllocationEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying TableAllocationEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying TableAllocationEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy

        /*if(!fieldsToEscape.contains("status") && StringUtils.hasText(StringUtils.trimWhitespace(form.getStatus()))) {
            TableAllocationStatus expectedStatus = TableAllocationStatus.valueOf(form.getStatus());
            TableAllocationHistoryDocument checkInHistoryDocument = actualEntity.getStatusHistory().get(actualEntity.getStatusHistory().size() - 1);
            if(expectedStatus.compareTo(checkInHistoryDocument.getStatus()) != 0) {
                expectedEntity.addStatus(expectedStatus);
                changeSW = true;
                log.debug("TableAllocationForm.status: {} is different as TableAllocationEntity.status: {}", form.getStatus(), checkInHistoryDocument.getStatus());
            } else {
                expectedEntity.setStatusHistory(actualEntity.getStatusHistory());
                log.debug("TableAllocationForm.status: is unchanged");
            }
        } else {
            expectedEntity.setStatusHistory(actualEntity.getStatusHistory());
            log.debug("TableAllocationForm.status: is unchanged");
        }*/

        if(!fieldsToEscape.contains("tableId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTableId()))
                && form.getTableId().compareTo(actualEntity.getTableId()) != 0) {
            expectedEntity.setTableId(form.getTableId());
            changeSW = true;
            log.debug("TableAllocationForm.tableId: {} is different as TableAllocationEntity.tableId: {}", form.getTableId(), actualEntity.getTableId());
        } else {
            expectedEntity.setTableId(actualEntity.getTableId());
            log.debug("TableAllocationForm.tableId: is unchanged");
        }

        if(!fieldsToEscape.contains("notes") && StringUtils.hasText(StringUtils.trimWhitespace(form.getNotes()))
                && form.getNotes().compareTo(actualEntity.getNotes()) != 0) {
            expectedEntity.setNotes(form.getNotes());
            changeSW = true;
            log.debug("TableAllocationForm.notes: {} is different as TableAllocationEntity.notes: {}", form.getNotes(), actualEntity.getNotes());
        } else {
            expectedEntity.setNotes(actualEntity.getNotes());
            log.debug("TableAllocationForm.notes: is unchanged");
        }

        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
