package com.teenthofabud.restaurant.solution.reservation.engagement.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementDto;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementEvent;
import com.teenthofabud.restaurant.solution.reservation.error.ReservationErrorCode;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingException;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.reservation.booking.service.BookingService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class EngagementDtoValidator implements Validator {

    private List<String> fieldsToEscape;
    private BookingService bookingService;
    private String dateFormat;
    private String timeFormat;
    private String timestampFormat;

    @Value("#{'${res.booking.engagement.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Value("${res.booking.engagement.time.format}")
    public void setTimeFormat(String timeFormat) {
        this.timeFormat = timeFormat;
    }

    @Value("${res.booking.engagement.date.format}")
    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    @Value("${res.booking.engagement.timestamp.format}")
    public void setTimestampFormat(String timestampFormat) {
        this.timestampFormat = timestampFormat;
    }

    @Autowired
    public void setAssociationService(BookingService bookingService) {
        this.bookingService = bookingService;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(EngagementDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        EngagementDto dto = (EngagementDto) target;

        Optional<String> optAssociationId = dto.getAssociationId();
        if(!fieldsToEscape.contains("associationId") && optAssociationId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optAssociationId.get()))) {
            errors.rejectValue("associationId", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            log.debug("EngagementDto.associationId is invalid");
            return;
        } else if(!fieldsToEscape.contains("associationId") && optAssociationId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optAssociationId.get()))) {
            String associationId = optAssociationId.get();
            try {
                BookingVo bookingVo = bookingService.retrieveDetailsById(associationId, Optional.of(TOABCascadeLevel.ONE));
                if(!bookingVo.getActive()) {
                    log.debug("EngagementDto.associationId is inactive");
                    errors.rejectValue("associationId", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (BookingException e) {
                log.debug("EngagementDto.associationId is invalid");
                errors.rejectValue("associationId", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<String> optEvent = dto.getEvent();
        if(!fieldsToEscape.contains("event") && optEvent.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optEvent.get()))) {
            errors.rejectValue("event", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            log.debug("EngagementDto.event is invalid");
            return;
        } else if(!fieldsToEscape.contains("event") && optEvent.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optEvent.get()))) {
            String event = optEvent.get();
            try {
                EngagementEvent.valueOf(event);
            } catch (IllegalArgumentException e) {
                log.debug("EngagementDto.associationId is invalid");
                errors.rejectValue("associationId", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<String> optDate = dto.getDate();
        if(!fieldsToEscape.contains("date") && optDate.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optDate.get()))) {
            errors.rejectValue("date", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            log.debug("EngagementDto.date is invalid");
            return;
        } else if(!fieldsToEscape.contains("date") && optDate.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optDate.get()))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(dateFormat);
                LocalDate.parse(optDate.get(), dtf);
            } catch (DateTimeParseException e) {
                errors.rejectValue("date", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                log.debug("EngagementDto.date is invalid");
                return;
            }
        }

        Optional<String> optTime = dto.getTime();
        if(!fieldsToEscape.contains("time") && optTime.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optTime.get()))) {
            errors.rejectValue("time", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            log.debug("EngagementDto.time is invalid");
            return;
        } else if(!fieldsToEscape.contains("time") && optTime.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optTime.get()))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(dateFormat);
                LocalTime.parse(optTime.get(), dtf);
            } catch (DateTimeParseException e) {
                errors.rejectValue("time", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                log.debug("EngagementDto.time is invalid");
                return;
            }
        }

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                log.debug("EngagementDto.active is invalid");
                return;
            }
        }
    }

}