package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class WalkInForm2EntityConverter extends CheckInForm2EntityConverter {

    private List<String> fieldsToEscape;

    @Value("#{'${res.engagement.checkIn.walkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected Optional<? extends CheckInEntity> convert(Optional<? extends CheckInFormParameters> optionalCheckInFormParameters) {
        WalkInForm form = (WalkInForm) optionalCheckInFormParameters.get();
        WalkInEntity entity = new WalkInEntity();
        if(!fieldsToEscape.contains("name")) {
            entity.setName(form.getName());
        }
        if(!fieldsToEscape.contains("phoneNumber")) {
            entity.setPhoneNumber(form.getPhoneNumber());
        }
        if(!fieldsToEscape.contains("emailId")) {
            entity.setEmailId(form.getEmailId());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return Optional.of(entity);
    }

}
