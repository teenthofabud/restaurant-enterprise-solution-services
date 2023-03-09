package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.convert.converter.Converter;

import java.util.List;

@Slf4j
public abstract class CheckInForm2EntityConverter<T extends CheckInForm, U extends CheckInEntity> implements Converter<T, U> {

    public abstract List<String> getFieldsToEscape();

    protected U convert(T form, U entity) {
        if(!getFieldsToEscape().contains("sequence")) {
            entity.setSequence(form.getSequence());
        }
        if(!getFieldsToEscape().contains("noOfPersons")) {
            entity.setNoOfPersons(form.getNoOfPersons());
        }
        if(!getFieldsToEscape().contains("accountId")) {
            entity.setAccountId(form.getAccountId());
        }
        if(!getFieldsToEscape().contains("notes")) {
            entity.setNotes(form.getNotes());
        }
        if(!getFieldsToEscape().contains("type")) {
            entity.setType(CheckInType.valueOf(form.getType()));
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
    
    /*@Override
    public U convert(T form) {
        CheckInEntity entity = new CheckInEntity();
        if(!getFieldsToEscape().contains("sequence")) {
            entity.setSequence(form.getSequence());
        }
        *//*if(!getFieldsToEscape().contains("tableId")) {
            entity.setTableId(form.getTableId());
        }
        if(!getFieldsToEscape().contains("status")) {
            CheckInStatus status = CheckInStatus.valueOf(form.getStatus());
            entity.addStatus(status);
        }*//*
        if(!getFieldsToEscape().contains("noOfPersons")) {
            entity.setNoOfPersons(form.getNoOfPersons());
        }
        if(!getFieldsToEscape().contains("accountId")) {
            entity.setAccountId(form.getAccountId());
        }
        *//*if(!getFieldsToEscape().contains("name")) {
            entity.setName(form.getName());
        }
        if(!getFieldsToEscape().contains("phoneNumber")) {
            entity.setPhoneNumber(form.getPhoneNumber());
        }
        if(!getFieldsToEscape().contains("emailId")) {
            entity.setEmailId(form.getEmailId());
        }*//*
        if(!getFieldsToEscape().contains("notes")) {
            entity.setNotes(form.getNotes());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        U child = this.convertChild((T) form, entity);
        return child;
    }*/

    //protected abstract U convertChild(T heckInFormChild, CheckInEntity checkInEntity);

}
