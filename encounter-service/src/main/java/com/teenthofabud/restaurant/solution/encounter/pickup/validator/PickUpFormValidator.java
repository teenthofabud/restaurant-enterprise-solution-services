package com.teenthofabud.restaurant.solution.encounter.pickup.validator;

import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingFormValidator;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpForm;
import constants.EncounterErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class PickUpFormValidator extends MeetingFormValidator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.encounter.meeting.pickUp.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public List<String> getFieldsToEscape() {
        return fieldsToEscape;
    }

    @Override
    protected void validate(Optional<? extends MeetingForm> optionalMeetingForm, Errors errors) {
        if(optionalMeetingForm.isEmpty()) {
            log.debug("No PickUpForm available");
            return;
        }
        MeetingForm target = optionalMeetingForm.get();
        PickUpForm form = (PickUpForm) target;

        if(!fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("PickUpForm.name is empty");
            errors.rejectValue("name", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            return;
        }
        log.debug("PickUpForm.name is invalid");

        if(!fieldsToEscape.contains("phoneNo") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getPhoneNo()))) {
            log.debug("PickUpForm.phoneNo is empty");
            errors.rejectValue("phoneNo", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            return;
        }
        log.debug("PickUpForm.phoneNo is invalid");
    }

}
