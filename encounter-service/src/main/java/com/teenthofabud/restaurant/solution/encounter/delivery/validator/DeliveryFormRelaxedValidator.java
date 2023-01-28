package com.teenthofabud.restaurant.solution.encounter.delivery.validator;

import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingFormRelaxedValidator;
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
public class DeliveryFormRelaxedValidator extends MeetingFormRelaxedValidator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.encounter.meeting.delivery.fields-to-escape}'.split(',')}")
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
            log.debug("No DeliveryForm available");
            return false;
        }
        MeetingForm meetingForm = optionalMeetingForm.get();
        DeliveryForm form = (DeliveryForm) meetingForm;
        log.debug("DeliveryForm is available");

        if(!fieldsToEscape.contains("orderId") && form.getOrderId() != null &&
                StringUtils.isEmpty(StringUtils.trimWhitespace(form.getOrderId()))) {
            errors.rejectValue("orderId", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            log.debug("DeliveryForm.orderId is empty");
            return false;
        }
        log.debug("DeliveryForm.orderId is valid");

        return true;
    }

}
