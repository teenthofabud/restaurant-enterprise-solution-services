package com.teenthofabud.restaurant.solution.reservation.booking.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingForm;
import com.teenthofabud.restaurant.solution.reservation.error.ReservationErrorCode;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.reservation.category.service.CategoryService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class BookingFormRelaxedValidator implements RelaxedValidator<BookingForm>  {

    private List<String> fieldsToEscape;
    //private Validator tableIdValidator;
    private Validator accountIdValidator;
    private CategoryService categoryService;
    private String bookingTimeFormat;

    @Value("${res.reservation.booking.timestamp}")
    public void setBookingTimeFormat(String bookingTimeFormat) {
        this.bookingTimeFormat = bookingTimeFormat;
    }

    @Value("#{'${res.reservation.booking.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    /*@Autowired
    @Qualifier("tableIdValidator")
    public void setTableIdValidator(Validator tableIdValidator) {
        this.tableIdValidator = tableIdValidator;
    }*/

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
    public Boolean validateLoosely(BookingForm form, Errors errors) {
        if(!fieldsToEscape.contains("categoryId") && form.getCategoryId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCategoryId()))) {
            log.debug("BookingForm.categoryId is empty");
            errors.rejectValue("categoryId", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("categoryId") && form.getCategoryId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getCategoryId()))){
            String categoryId = form.getCategoryId();
            try {
                CategoryVo categoryVo = categoryService.retrieveDetailsById(categoryId, Optional.of(TOABCascadeLevel.ONE));
                if(!categoryVo.getActive()) {
                    log.debug("BookingForm.categoryId is inactive");
                    errors.rejectValue("categoryId", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                    return false;
                }
            } catch (CategoryException e) {
                log.debug("BookingForm.categoryId is invalid");
                errors.rejectValue("categoryId", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("BookingForm.categoryId is valid");

        if(!fieldsToEscape.contains("timestamp") && form.getTimestamp() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTimestamp()))) {
            errors.rejectValue("timestamp", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("BookingForm.timestamp is invalid");
            return false;
        } else if(!fieldsToEscape.contains("timestamp") && form.getTimestamp() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getTimestamp()))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(bookingTimeFormat);
                dtf.parse(form.getTimestamp());
            } catch (DateTimeParseException e) {
                log.debug("BookingForm.timestamp is invalid");
                errors.rejectValue("timestamp", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("BookingForm.timestamp is valid");


        if(!fieldsToEscape.contains("noOfPerson") && (form.getNoOfPerson() != null || form.getNoOfPerson() <= 0)) {
            errors.rejectValue("noOfPerson", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("ReservationForm.noOfPerson is invalid");
            return false;
        }
        log.debug("BookingForm.noOfPerson is valid");


        /*if(!fieldsToEscape.contains("tableId") && form.getTableId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTableId()))) {
            log.debug("ReservationForm.tableId is empty");
            errors.rejectValue("tableId", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("tableId") && form.getTableId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getTableId()))){
            Errors err = new DirectFieldBindingResult(form.getTableId(), "ReservationForm");
            tableIdValidator.validate(form.getTableId(), err);
            if(err.hasErrors()) {
                log.debug("ReservationForm.tableId is invalid");
                ReservationErrorCode ec = ReservationErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("ReservationForm error detail: {}", ec);
                errors.rejectValue("tableId", ec.name());
                return false;
            }
        }
        log.debug("ReservationForm.tableId is valid");*/

        if(!fieldsToEscape.contains("accountId") && form.getAccountId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAccountId()))) {
            log.debug("BookingForm.accountId is empty");
            errors.rejectValue("accountId", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("accountId") && form.getAccountId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))){
            Errors err = new DirectFieldBindingResult(form.getAccountId(), "ReservationForm");
            accountIdValidator.validate(form.getAccountId(), err);
            if(err.hasErrors()) {
                log.debug("BookingForm.accountId is invalid");
                ReservationErrorCode ec = ReservationErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("BookingForm error detail: {}", ec);
                errors.rejectValue("accountId", ec.name());
                return false;
            }
        }
        log.debug("BookingForm.accountId is valid");

        return true;
    }
}
