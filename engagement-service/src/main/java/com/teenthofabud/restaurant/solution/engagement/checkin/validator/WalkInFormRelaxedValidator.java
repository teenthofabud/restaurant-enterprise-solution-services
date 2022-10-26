package com.teenthofabud.restaurant.solution.engagement.checkin.validator;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInForm;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class WalkInFormRelaxedValidator extends CheckInFormRelaxedValidator  {

    private List<String> fieldsToEscape;

    @Value("#{'${res.engagement.checkIn.walkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Boolean validateLoosely(Optional<? extends CheckInForm> optionalCheckInForm, Errors errors) {
        if(optionalCheckInForm.isEmpty()) {
            log.debug("No WalkInForm available");
            return false;
        }
        CheckInForm checkInForm = optionalCheckInForm.get();
        WalkInForm form = (WalkInForm) checkInForm;
        log.debug("WalkInForm is available");

        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("WalkInForm.name is empty");
            return false;
        }
        log.debug("WalkInForm.timestamp is valid");

        if(!fieldsToEscape.contains("phoneNumber") && form.getPhoneNumber() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getPhoneNumber()))) {
            errors.rejectValue("phoneNumber", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("WalkInForm.phoneNumber is empty");
            return false;
        }
        log.debug("WalkInForm.phoneNumber is valid");

        if(!fieldsToEscape.contains("emailId") && form.getEmailId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getEmailId()))) {
            errors.rejectValue("emailId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("WalkInForm.emailId is empty");
            return false;
        }
        log.debug("WalkInForm.emailId is valid");

        return true;
    }

}
