package com.teenthofabud.restaurant.solution.reservation.engagement.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingException;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.reservation.booking.service.BookingService;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementEvent;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementForm;
import com.teenthofabud.restaurant.solution.reservation.error.ReservationErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
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
public class EngagementFormRelaxedValidator implements RelaxedValidator<EngagementForm>  {

    private List<String> fieldsToEscape;
    private BookingService bookingService;
    private String dateFormat;
    private String timeFormat;

    @Value("#{'${res.booking.engagement.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Value("${res.booking.engagement.date.format}")
    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    @Value("${res.booking.engagement.time.format}")
    public void setTimeFormat(String timeFormat) {
        this.timeFormat = timeFormat;
    }

    @Autowired
    public void setAssociationService(BookingService bookingService) {
        this.bookingService = bookingService;
    }

    @Override
    public Boolean validateLoosely(EngagementForm form, Errors errors) {
        if(!fieldsToEscape.contains("associationId") && form.getAssociationId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAssociationId()))) {
            log.debug("EngagementForm.associationId is empty");
            errors.rejectValue("associationId", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("associationId") && form.getAssociationId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getAssociationId()))){
            String associationId = form.getAssociationId();
            try {
                BookingVo bookingVo = bookingService.retrieveDetailsById(associationId, Optional.of(TOABCascadeLevel.ONE));
                if(!bookingVo.getActive()) {
                    log.debug("EngagementForm.associationId is inactive");
                    errors.rejectValue("associationId", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                    return false;
                }
            } catch (BookingException e) {
                log.debug("EngagementForm.associationId is invalid");
                errors.rejectValue("associationId", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("EngagementForm.associationId is valid");

        if(!fieldsToEscape.contains("event") && form.getEvent() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getEvent()))) {
            log.debug("EngagementForm.event is empty");
            errors.rejectValue("event", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("event") && form.getEvent() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getEvent()))){
            try {
                EngagementEvent.valueOf(form.getEvent());
            } catch (IllegalArgumentException e) {
                log.debug("EngagementDto.event is invalid");
                errors.rejectValue("event", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("EngagementForm.tableId is valid");

        if(!fieldsToEscape.contains("date") && form.getDate() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDate()))) {
            errors.rejectValue("date", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            log.debug("EngagementForm.date is empty");
            return false;
        } else if(!fieldsToEscape.contains("date") && form.getDate() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getDate()))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(dateFormat);
                LocalDate.parse(form.getDate(), dtf);
            } catch (DateTimeParseException e) {
                errors.rejectValue("date", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                log.debug("EngagementForm.date is invalid");
                return false;
            }
        }
        log.debug("EngagementForm.date is valid");

        if(!fieldsToEscape.contains("time") && form.getTime() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTime()))) {
            errors.rejectValue("time", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            log.debug("EngagementForm.time is empty");
            return false;
        } else if(!fieldsToEscape.contains("time") && form.getTime() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getTime()))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(timeFormat);
                LocalTime.parse(form.getTime(), dtf);
            } catch (DateTimeParseException e) {
                errors.rejectValue("time", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                log.debug("EngagementForm.time is invalid");
                return false;
            }
        }
        log.debug("EngagementForm.time is valid");

        return true;
    }
}
