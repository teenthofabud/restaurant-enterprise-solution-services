package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;

import java.util.List;

@Slf4j
public abstract class CheckInForm2EntityConverter<T extends CheckInForm, U extends CheckInEntity> implements Converter<CheckInForm, CheckInEntity> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.engagement.checkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public CheckInEntity convert(CheckInForm form) {
        CheckInEntity entity = new CheckInEntity();
        if(!fieldsToEscape.contains("sequence")) {
            entity.setSequence(form.getSequence());
        }
        /*if(!fieldsToEscape.contains("tableId")) {
            entity.setTableId(form.getTableId());
        }
        if(!fieldsToEscape.contains("status")) {
            CheckInStatus status = CheckInStatus.valueOf(form.getStatus());
            entity.addStatus(status);
        }*/
        if(!fieldsToEscape.contains("noOfPersons")) {
            entity.setNoOfPersons(form.getNoOfPersons());
        }
        if(!fieldsToEscape.contains("accountId")) {
            entity.setAccountId(form.getAccountId());
        }
        /*if(!fieldsToEscape.contains("name")) {
            entity.setName(form.getName());
        }
        if(!fieldsToEscape.contains("phoneNumber")) {
            entity.setPhoneNumber(form.getPhoneNumber());
        }
        if(!fieldsToEscape.contains("emailId")) {
            entity.setEmailId(form.getEmailId());
        }*/
        if(!fieldsToEscape.contains("notes")) {
            entity.setNotes(form.getNotes());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        U child = this.convertChild((T) form, entity);
        return child;
    }

    protected abstract U convertChild(T heckInFormChild, CheckInEntity checkInEntity);

}
