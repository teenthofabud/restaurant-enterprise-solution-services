package com.teenthofabud.restaurant.solution.encounter.delivery.validator;

import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryDto;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingDto;
import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingDtoValidator;
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
public class DeliveryDtoValidator extends MeetingDtoValidator {

    private List<String> fieldsToEscape;
    @Value("#{'${res.encounter.meeting.delivery.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected void validate(Optional<? extends MeetingDto> optionalMeetingDto, Errors errors) {
        if(optionalMeetingDto.isEmpty()) {
            log.debug("No DeliveryDto available");
            return;
        }
        MeetingDto checkInDto = optionalMeetingDto.get();
        DeliveryDto dto = (DeliveryDto) checkInDto;

        Optional<String> optOrderId = dto.getOrderId();
        if(!fieldsToEscape.contains("orderId") && optOrderId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optOrderId.get()))) {
            errors.rejectValue("orderId", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            log.debug("DeliveryDto.orderId is invalid");
            return;
        }
    }

}
