package com.teenthofabud.restaurant.solution.engagement.tableallocation.converter;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.CheckInRepository;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationEntity;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class TableAllocationForm2EntityConverter implements Converter<TableAllocationForm, TableAllocationEntity> {

    private List<String> fieldsToEscape;

    private CheckInRepository checkInRepository;

    @Autowired
    public void setCheckInRepository(CheckInRepository checkInRepository) {
        this.checkInRepository = checkInRepository;
    }

    @Value("#{'${res.engagement.tableAllocation.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public TableAllocationEntity convert(TableAllocationForm form) {
        TableAllocationEntity entity = new TableAllocationEntity();
        if(!fieldsToEscape.contains("accountId")) {
            entity.setTableId(form.getTableId());
        }
        if(!fieldsToEscape.contains("notes")) {
            entity.setNotes(form.getNotes());
        }
        if(!fieldsToEscape.contains("notes")) {
            entity.setNotes(form.getNotes());
        }
        if(!fieldsToEscape.contains("checkInId")) {
            Long checkInId = Long.parseLong(form.getCheckInId());
            Optional<CheckInEntity> checkInEntityOptional = checkInRepository.findById(checkInId);
            entity.setCheckIn(checkInEntityOptional.get());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
    
    /*@Override
    public U convert(T form) {
        TableAllocationEntity entity = new TableAllocationEntity();
        if(!fieldsToEscape.contains("sequence")) {
            entity.setSequence(form.getSequence());
        }
        *//*if(!fieldsToEscape.contains("tableId")) {
            entity.setTableId(form.getTableId());
        }
        if(!fieldsToEscape.contains("status")) {
            TableAllocationStatus status = TableAllocationStatus.valueOf(form.getStatus());
            entity.addStatus(status);
        }*//*
        if(!fieldsToEscape.contains("noOfPersons")) {
            entity.setNoOfPersons(form.getNoOfPersons());
        }
        if(!fieldsToEscape.contains("accountId")) {
            entity.setAccountId(form.getAccountId());
        }
        *//*if(!fieldsToEscape.contains("name")) {
            entity.setName(form.getName());
        }
        if(!fieldsToEscape.contains("phoneNumber")) {
            entity.setPhoneNumber(form.getPhoneNumber());
        }
        if(!fieldsToEscape.contains("emailId")) {
            entity.setEmailId(form.getEmailId());
        }*//*
        if(!fieldsToEscape.contains("notes")) {
            entity.setNotes(form.getNotes());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        U child = this.convertChild((T) form, entity);
        return child;
    }*/

    //protected abstract U convertChild(T heckInFormChild, TableAllocationEntity checkInEntity);

}
