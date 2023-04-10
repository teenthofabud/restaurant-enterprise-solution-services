package com.teenthofabud.restaurant.solution.engagement.checkin.validator;

import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInDto;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationDto;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class ReservationDtoValidator extends CheckInDtoValidator {

    private List<String> fieldsToEscape;
    private String reservationDateFormat;
    private String reservationTimeFormat;

    @Value("${res.engagement.checkIn.reservation.time.format}")
    public void setReservationTimeFormat(String reservationTimeFormat) {
        this.reservationTimeFormat = reservationTimeFormat;
    }

    @Value("${res.engagement.checkIn.reservation.date.format}")
    public void setReservationDateFormat(String reservationDateFormat) {
        this.reservationDateFormat = reservationDateFormat;
    }

    @Value("#{'${res.engagement.checkIn.reservation.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public List<String> getFieldsToEscape() {
        return this.fieldsToEscape;
    }

    @Override
    protected void validate(Optional<? extends CheckInDto> optionalCheckInDtoParameters, Errors errors) {
        if(optionalCheckInDtoParameters.isEmpty()) {
            log.debug("No ReservationDto available");
            return;
        }
        CheckInDto target = optionalCheckInDtoParameters.get();
        ReservationDto dto = (ReservationDto) target;

        Optional<String> optDate = dto.getDate();
        if(!fieldsToEscape.contains("date") && optDate.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optDate.get()))) {
            errors.rejectValue("date", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("ReservationDto.date is empty");
            return;
        } else if(!fieldsToEscape.contains("date") && optDate.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optDate.get()))) {
            try {
                LocalDate.parse(optDate.get(), DateTimeFormatter.ofPattern(reservationDateFormat));
            } catch (DateTimeParseException w)  {
                log.debug("ReservationDto.date is invalid");
                errors.rejectValue("date", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<String> optTime = dto.getTime();
        if(!fieldsToEscape.contains("time") && optTime.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optTime.get()))) {
            errors.rejectValue("time", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("ReservationDto.time is empty");
            return;
        } else  if(!fieldsToEscape.contains("time") && optTime.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optTime.get()))) {
            try {
                LocalTime.parse(optTime.get(), DateTimeFormatter.ofPattern(reservationTimeFormat));
            } catch (DateTimeParseException w)  {
                log.debug("ReservationDto.time is invalid");
                errors.rejectValue("time", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                return;
            }
        }
    }

    @Override
    protected CheckInType getCheckInTypeInContext() {
        return CheckInType.RESERVATION;
    }

}
