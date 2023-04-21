package com.teenthofabud.restaurant.solution.engagement.checkin.validator;

import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationForm;
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
public class ReservationFormRelaxedValidator extends CheckInFormRelaxedValidator  {

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
    public Boolean validateLoosely(Optional<? extends CheckInForm> optionalCheckInForm, Errors errors) {
        if(!optionalCheckInForm.isPresent()) {
            log.debug("No ReservationForm available");
            return false;
        }
        CheckInForm checkInForm = optionalCheckInForm.get();
        ReservationForm form = (ReservationForm) checkInForm;
        log.debug("ReservationForm is available");

        if (!fieldsToEscape.contains("date") && form.getDate() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDate()))) {
            errors.rejectValue("date", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("ReservationForm.date is empty");
            return false;
        } else if (!fieldsToEscape.contains("date") && form.getDate() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getDate()))) {
            try {
                LocalDate.parse(form.getDate(), DateTimeFormatter.ofPattern(reservationDateFormat));
            } catch (DateTimeParseException w) {
                log.debug("ReservationForm.date is invalid");
                errors.rejectValue("date", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("ReservationForm.date is valid");

        if (!fieldsToEscape.contains("time") && form.getTime() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTime()))) {
            errors.rejectValue("time", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("ReservationForm.time is empty");
            return false;
        } else if (!fieldsToEscape.contains("time") && form.getTime() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getTime()))) {
            try {
                LocalTime.parse(form.getTime(), DateTimeFormatter.ofPattern(reservationTimeFormat));
            } catch (DateTimeParseException w) {
                log.debug("ReservationForm.time is invalid");
                errors.rejectValue("time", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("ReservationForm.time is valid");

        return true;
    }
    @Override
    public List<String> getFieldsToEscape() {
        return this.fieldsToEscape;
    }

    @Override
    protected CheckInType getCheckInTypeInContext() {
        return CheckInType.RESERVATION;
    }

}
