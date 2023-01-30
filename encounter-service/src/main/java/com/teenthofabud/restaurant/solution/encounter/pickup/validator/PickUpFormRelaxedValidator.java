package com.teenthofabud.restaurant.solution.encounter.pickup.validator;

import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpForm;
import com.teenthofabud.restaurant.solution.encounter.constants.EncounterErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class PickUpFormRelaxedValidator extends MeetingFormRelaxedValidator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.encounter.meeting.pickUp.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public List<String> getFieldsToEscape() {
        return this.fieldsToEscape;
    }

    @Override
    public Boolean validateLoosely(Optional<? extends MeetingForm> optionalMeetingForm, Errors errors) {
        if(optionalMeetingForm.isEmpty()) {
            log.debug("No PickUpForm available");
            return false;
        }
        MeetingForm meetingForm = optionalMeetingForm.get();
        PickUpForm form = (PickUpForm) meetingForm;
        log.debug("PickUpForm is available");

        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            log.debug("PickUpForm.name is empty");
            return false;
        }
        log.debug("PickUpForm.name is valid");

        if(!fieldsToEscape.contains("phoneNo") && form.getPhoneNo() != null &&
                StringUtils.isEmpty(StringUtils.trimWhitespace(form.getPhoneNo()))) {
            errors.rejectValue("phoneNo", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            log.debug("PickUpForm.phoneNo is empty");
            return false;
        }
        log.debug("PickUpForm.phoneNo is valid");

        return true;
    }

}
