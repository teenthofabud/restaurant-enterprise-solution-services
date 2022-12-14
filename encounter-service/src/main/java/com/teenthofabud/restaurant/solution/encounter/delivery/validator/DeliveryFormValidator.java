package com.teenthofabud.restaurant.solution.encounter.delivery.validator;

import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingFormValidator;
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
public class DeliveryFormValidator extends MeetingFormValidator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.encounter.meeting.delivery.fields-to-escape}'.split(',')}")
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
            log.debug("No DeliveryForm available");
            return;
        }
        MeetingForm target = optionalMeetingForm.get();
        DeliveryForm form = (DeliveryForm) target;

        if(!fieldsToEscape.contains("orderId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getOrderId()))) {
            log.debug("DeliveryForm.orderId is empty");
            errors.rejectValue("orderId", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            return;
        }
        log.debug("DeliveryForm.orderId is invalid");
    }

}
