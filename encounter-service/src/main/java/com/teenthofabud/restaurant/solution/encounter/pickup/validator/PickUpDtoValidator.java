package com.teenthofabud.restaurant.solution.encounter.pickup.validator;

import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingDto;
import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingDtoValidator;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpDto;
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
public class PickUpDtoValidator extends MeetingDtoValidator {

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
    protected void validate(Optional<? extends MeetingDto> optionalMeetingDto, Errors errors) {
        if(!optionalMeetingDto.isPresent()) {
            log.debug("No PickUpDto available");
            return;
        }
        MeetingDto checkInDto = optionalMeetingDto.get();
        PickUpDto dto = (PickUpDto) checkInDto;

        Optional<String> optName = dto.getName();
        if(optName.isPresent() && !fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            log.debug("PickUpDto.name is invalid");
            return;
        }

        Optional<String> optPhoneNumber = dto.getPhoneNo();
        if(optPhoneNumber.isPresent() && !fieldsToEscape.contains("phoneNo") && StringUtils.isEmpty(StringUtils.trimWhitespace(optPhoneNumber.get()))) {
            errors.rejectValue("phoneNo", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            log.debug("PickUpDto.phoneNo is invalid");
            return;
        }
    }

}
