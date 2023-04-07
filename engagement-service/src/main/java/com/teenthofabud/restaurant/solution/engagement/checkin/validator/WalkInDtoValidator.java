package com.teenthofabud.restaurant.solution.engagement.checkin.validator;

import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInDto;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInDto;
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
public class WalkInDtoValidator extends CheckInDtoValidator {

    private List<String> fieldsToEscape;
    @Value("#{'${res.engagement.checkIn.walkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected void validate(Optional<? extends CheckInDto> optionalCheckInDto, Errors errors) {
        if(optionalCheckInDto.isEmpty()) {
            log.debug("No WalkInDto available");
            return;
        }
        CheckInDto checkInDto = optionalCheckInDto.get();
        WalkInDto dto = (WalkInDto) checkInDto;

        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("WalkInDto.name is invalid");
            return;
        }

        Optional<String> optEmailId = dto.getEmailId();
        if(!fieldsToEscape.contains("emailId") && optEmailId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optEmailId.get()))) {
            errors.rejectValue("emailId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("WalkInDto.emailId is invalid");
            return;
        }

        Optional<String> optPhoneNumber = dto.getPhoneNumber();
        if(!fieldsToEscape.contains("phoneNumber") && optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optPhoneNumber.get()))) {
            errors.rejectValue("phoneNumber", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("WalkInDto.phoneNumber is invalid");
            return;
        }
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
