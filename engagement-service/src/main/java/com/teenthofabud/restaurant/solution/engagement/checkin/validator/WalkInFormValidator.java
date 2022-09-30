package com.teenthofabud.restaurant.solution.engagement.checkin.validator;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInFormParameters;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInForm;
import com.teenthofabud.restaurant.solution.engagement.error.EngagementErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class WalkInFormValidator extends CheckInFormValidator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.engagement.checkIn.walkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected void validate(Optional<? extends CheckInFormParameters> optionalCheckInFormParameters, Errors errors) {
        if(optionalCheckInFormParameters.isEmpty()) {
            log.debug("No WalkInForm available");
            return;
        }
        CheckInFormParameters target = optionalCheckInFormParameters.get();
        WalkInForm form = (WalkInForm) target;

        if(!fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("WalkInForm.name is empty");
            errors.rejectValue("name", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return;
        }
        log.debug("WalkInForm.name is invalid");

        if(!fieldsToEscape.contains("emailId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getEmailId()))) {
            log.debug("WalkInForm.emailId is empty");
            errors.rejectValue("emailId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return;
        }
        log.debug("WalkInForm.emailId is invalid");

        if(!fieldsToEscape.contains("phoneNumber") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getPhoneNumber()))) {
            log.debug("WalkInForm.phoneNumber is empty");
            errors.rejectValue("phoneNumber", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return;
        }
        log.debug("WalkInForm.phoneNumber is invalid");
    }

}
