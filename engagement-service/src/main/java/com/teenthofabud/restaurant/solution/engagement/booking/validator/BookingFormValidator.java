package com.teenthofabud.restaurant.solution.engagement.booking.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.engagement.booking.data.BookingForm;
import com.teenthofabud.restaurant.solution.checkin.error.CheckInErrorCode;
import com.teenthofabud.restaurant.solution.engagement.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.engagement.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.engagement.category.service.CategoryService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class BookingFormValidator implements Validator {

    private List<String> fieldsToEscape;
    //private String endedOnFormat;
    //private Validator tableIdValidator;
    private Validator accountIdValidator;
    private CategoryService categoryService;
    private String bookingTimeFormat;

    @Value("#{'${res.reservation.booking.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    /*@Autowired
    @Qualifier("tableIdValidator")
    public void setTableIdValidator(Validator tableIdValidator) {
        this.tableIdValidator = tableIdValidator;
    }*/

    @Value("${res.reservation.booking.timestamp}")
    public void setBookingTimeFormat(String bookingTimeFormat) {
        this.bookingTimeFormat = bookingTimeFormat;
    }

    @Autowired
    @Qualifier("accountIdValidator")
    public void setAccountIdValidator(Validator accountIdValidator) {
        this.accountIdValidator = accountIdValidator;
    }

    @Autowired
    public void setCategoryService(CategoryService categoryService) {
        this.categoryService = categoryService;
    }
    
    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(BookingForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        BookingForm form = (BookingForm) target;

        if(!fieldsToEscape.contains("categoryId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCategoryId()))) {
            log.debug("BookingForm.categoryId is empty");
            errors.rejectValue("categoryId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("categoryId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCategoryId()))){
            String categoryId = form.getCategoryId();
            try {
                CategoryVo categoryVo = categoryService.retrieveDetailsById(categoryId, Optional.of(TOABCascadeLevel.ONE));
                if(!categoryVo.getActive()) {
                    log.debug("BookingForm.categoryId is inactive");
                    errors.rejectValue("categoryId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (CategoryException e) {
                log.debug("BookingForm.categoryId is invalid");
                errors.rejectValue("categoryId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("BookingForm.categoryId is valid");

        if(!fieldsToEscape.contains("timestamp") && form.getTimestamp() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTimestamp()))) {
            errors.rejectValue("timestamp", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("BookingForm.timestamp is invalid");
            return;
        } else if(!fieldsToEscape.contains("timestamp") && form.getTimestamp() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getTimestamp()))) {
            try {
                LocalDateTime dt = LocalDateTime.parse(form.getTimestamp(), DateTimeFormatter.ofPattern(bookingTimeFormat));
                if(dt.isBefore(LocalDateTime.now())) {
                    log.debug("BookingForm.timestamp is in past");
                    errors.rejectValue("timestamp", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (DateTimeParseException e) {
                log.debug("BookingForm.timestamp is invalid");
                errors.rejectValue("timestamp", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("BookingForm.timestamp is valid");


        /*if(!fieldsToEscape.contains("noOfPerson") && (form.getNoOfPerson() != null || form.getNoOfPerson() <= 0)) {
            errors.rejectValue("noOfPerson", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("BookingForm.noOfPerson is invalid");
            return;
        }
        log.debug("BookingForm.noOfPerson is valid");*/

        /*if(!fieldsToEscape.contains("tableId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTableId()))) {
            log.debug("BookingForm.tableId is empty");
            errors.rejectValue("tableId", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("tableId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTableId()))){
            Errors err = new DirectFieldBindingResult(form.getTableId(), "BookingForm");
            tableIdValidator.validate(form.getTableId(), err);
            if(err.hasErrors()) {
                log.debug("BookingForm.tableId is invalid");
                ReservationErrorCode ec = ReservationErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("BookingForm error detail: {}", ec);
                errors.rejectValue("tableId", ec.name());
                return;
            }
        }
        log.debug("BookingForm.tableId is valid");*/

        if(!fieldsToEscape.contains("accountId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAccountId()))) {
            log.debug("BookingForm.accountId is empty");
            errors.rejectValue("accountId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("accountId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))){
            Errors err = new DirectFieldBindingResult(form.getAccountId(), "BookingForm");
            accountIdValidator.validate(form.getAccountId(), err);
            if(err.hasErrors()) {
                log.debug("BookingForm.accountId is invalid");
                CheckInErrorCode ec = CheckInErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("BookingForm error detail: {}", ec);
                errors.rejectValue("accountId", ec.name());
                return;
            }
        }
        log.debug("BookingForm.accountId is valid");
    }

}
