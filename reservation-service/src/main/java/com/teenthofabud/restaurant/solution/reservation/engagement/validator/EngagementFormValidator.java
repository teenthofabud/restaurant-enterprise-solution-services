package com.teenthofabud.restaurant.solution.reservation.engagement.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementForm;
import com.teenthofabud.restaurant.solution.reservation.error.ReservationErrorCode;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingException;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.reservation.booking.service.BookingService;
import com.teenthofabud.restaurant.solution.reservation.integration.establishmentarea.validator.TableIdValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class EngagementFormValidator implements Validator {

    private List<String> fieldsToEscape;
    /*private String dateFormat;
    private String timeFormat;*/
    private BookingService bookingService;
    private TableIdValidator tableIdValidator;

    @Value("#{'${res.booking.engagement.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    /*@Value("${res.booking.engagement.date.format}")
    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    @Value("${res.booking.engagement.time.format}")
    public void setTimeFormat(String timeFormat) {
        this.timeFormat = timeFormat;
    }*/

    @Autowired
    public void setBookingService(BookingService bookingService) {
        this.bookingService = bookingService;
    }

    @Autowired
    public void setTableIdValidator(TableIdValidator tableIdValidator) {
        this.tableIdValidator = tableIdValidator;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(EngagementForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        EngagementForm form = (EngagementForm) target;
        Optional<BookingVo> optionalBookingVo = Optional.empty();

        if(!fieldsToEscape.contains("bookingId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getBookingId()))) {
            errors.rejectValue("bookingId", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("EngagementForm.bookingId is invalid");
            return;
        } else if(!fieldsToEscape.contains("bookingId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getBookingId()))) {
            try {
                BookingVo bookingVo = bookingService.retrieveDetailsById(form.getBookingId(), Optional.of(TOABCascadeLevel.TWO));
                if(!bookingVo.getActive()) {
                    log.debug("EngagementForm.bookingId is inactive");
                    errors.rejectValue("bookingId", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                    return;
                }
                optionalBookingVo = Optional.of(bookingVo);
            } catch (BookingException e) {
                log.debug("EngagementForm.bookingId is invalid");
                errors.rejectValue("bookingId", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("EngagementForm.bookingId is valid");

        if(!fieldsToEscape.contains("tokenNumber") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTokenNumber()))) {
            errors.rejectValue("tokenNumber", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("EngagementDto.tokenNumber is invalid");
            return;
        }
        log.debug("EngagementForm.tokenNumber is valid");

        if(optionalBookingVo.isPresent()) {
            BookingVo bookingVo = optionalBookingVo.get();
            CategoryVo categoryVo = bookingVo.getCategory();
            switch (categoryVo.getName()) {
                case "Dine In":
                    if(!fieldsToEscape.contains("tableId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTableId()))) {
                        errors.rejectValue("tableId", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                        log.debug("EngagementForm.tableId is invalid");
                        return;
                    } else if(!fieldsToEscape.contains("tableId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTableId()))) {
                        Errors err = new DirectFieldBindingResult(form.getTableId(), "EngagementForm");
                        tableIdValidator.validate(form.getTableId(), err);
                        if(err.hasErrors()) {
                            log.debug("EngagementForm.tableId is invalid");
                            ReservationErrorCode ec = ReservationErrorCode.valueOf(err.getGlobalError().getCode());
                            log.debug("EngagementForm error detail: {}", ec);
                            errors.rejectValue("tableId", ec.name());
                            return;
                        }
                    }
                    log.debug("EngagementForm.tableId is valid");

                    if(!fieldsToEscape.contains("noOfPersons") && (form.getNoOfPersons() == null || form.getNoOfPersons() <= 0)) {
                        errors.rejectValue("noOfPersons", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                        log.debug("EngagementForm.noOfPersons is invalid");
                        return;
                    }
                    log.debug("EngagementForm.noOfPersons is valid");
                    break;
                case "Take Away":
                    break;
                case "Delivery":
                    if(!fieldsToEscape.contains("extRef") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getExtRef()))) {
                        errors.rejectValue("extRef", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                        log.debug("EngagementForm.extRef is invalid");
                        return;
                    }
                    log.debug("EngagementForm.extRef is valid");
                    break;
                default:
                    errors.rejectValue("bookingId", ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED.name());
                    log.debug("EngagementForm.bookingId is invalid");
                    return;
            }
        } else {
            errors.rejectValue("bookingId", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("EngagementForm.bookingId is invalid");
            return;
        }

    }

}
