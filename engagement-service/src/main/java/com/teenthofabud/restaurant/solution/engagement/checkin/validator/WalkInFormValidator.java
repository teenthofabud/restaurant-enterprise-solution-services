package com.teenthofabud.restaurant.solution.engagement.checkin.validator;

import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInForm;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.validator.routines.EmailValidator;
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

    private String phoneNumberRegex;

    @Value("${res.engagement.checkIn.walkIn.phoneNumber.regex}")
    public void setPhoneNumberRegex(String phoneNumberRegex) {
        this.phoneNumberRegex = phoneNumberRegex;
    }

    @Value("#{'${res.engagement.checkIn.walkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected void validate(Optional<? extends CheckInForm> optionalCheckInForm, Errors errors) {
        if(!optionalCheckInForm.isPresent()) {
            log.debug("No WalkInForm available");
            return;
        }
        CheckInForm target = optionalCheckInForm.get();
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
        } else if(!fieldsToEscape.contains("emailId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getEmailId())) && !EmailValidator.getInstance().isValid(form.getEmailId())) {
            log.debug("WalkInForm.emailId is invalid");
            errors.rejectValue("emailId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return;
        }
        log.debug("WalkInForm.emailId is invalid");

        if(!fieldsToEscape.contains("phoneNumber") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getPhoneNumber()))) {
            log.debug("WalkInForm.phoneNumber is empty");
            errors.rejectValue("phoneNumber", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("phoneNumber") && StringUtils.hasText(StringUtils.trimWhitespace(form.getEmailId())) && !form.getPhoneNumber().matches(phoneNumberRegex)) {
            log.debug("WalkInForm.phoneNumber is invalid");
            errors.rejectValue("phoneNumber", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return;
        }
        log.debug("WalkInForm.phoneNumber is invalid");
    }

    @Override
    public List<String> getFieldsToEscape() {
        return this.fieldsToEscape;
    }

    @Override
    protected CheckInType getCheckInTypeInContext() {
        return CheckInType.WALK_IN;
    }

}
