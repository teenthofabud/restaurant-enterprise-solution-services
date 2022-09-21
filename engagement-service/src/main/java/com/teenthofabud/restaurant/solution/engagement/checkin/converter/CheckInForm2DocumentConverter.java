package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInDocument;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInStatus;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class CheckInForm2DocumentConverter implements Converter<CheckInForm, CheckInDocument> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.reservation.booking.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public CheckInDocument convert(CheckInForm form) {
        CheckInDocument document = new CheckInDocument();
        if(!fieldsToEscape.contains("sequence")) {
            document.setSequence(form.getSequence());
        }
        if(!fieldsToEscape.contains("tableId")) {
            document.setTableId(form.getTableId());
        }
        if(!fieldsToEscape.contains("status")) {
            CheckInStatus status = CheckInStatus.valueOf(form.getStatus());
            document.addStatus(status);
        }
        if(!fieldsToEscape.contains("noOfPersons")) {
            document.setNoOfPersons(form.getNoOfPersons());
        }
        if(!fieldsToEscape.contains("accountId")) {
            document.setAccountId(form.getAccountId());
        }
        if(!fieldsToEscape.contains("name")) {
            document.setName(form.getName());
        }
        if(!fieldsToEscape.contains("phoneNumber")) {
            document.setPhoneNumber(form.getPhoneNumber());
        }
        if(!fieldsToEscape.contains("emailId")) {
            document.setEmailId(form.getEmailId());
        }
        if(!fieldsToEscape.contains("notes")) {
            document.setNotes(form.getNotes());
        }
        document.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, document);
        return document;
    }

}
