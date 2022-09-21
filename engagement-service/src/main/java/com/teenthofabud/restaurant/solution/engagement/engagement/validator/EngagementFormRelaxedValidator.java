package com.teenthofabud.restaurant.solution.engagement.engagement.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.engagement.booking.data.BookingException;
import com.teenthofabud.restaurant.solution.engagement.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.engagement.booking.service.BookingService;
import com.teenthofabud.restaurant.solution.engagement.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.EngagementForm;
import com.teenthofabud.restaurant.solution.checkin.error.CheckInErrorCode;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.validator.TableIdValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class EngagementFormRelaxedValidator implements RelaxedValidator<EngagementForm>  {

    private List<String> fieldsToEscape;
    private BookingService bookingService;
    private TableIdValidator tableIdValidator;
    /*private String dateFormat;
    private String timeFormat;*/

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
    public Boolean validateLoosely(EngagementForm form, Errors errors) {
        Optional<BookingVo> optionalBookingVo = Optional.empty();

        if(!fieldsToEscape.contains("bookingId") && form.getBookingId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getBookingId()))) {
            log.debug("EngagementForm.bookingId is empty");
            errors.rejectValue("bookingId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("bookingId") && form.getBookingId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getBookingId()))){
            String associationId = form.getBookingId();
            try {
                BookingVo bookingVo = bookingService.retrieveDetailsById(associationId, Optional.of(TOABCascadeLevel.ONE));
                if(!bookingVo.getActive()) {
                    log.debug("EngagementForm.bookingId is inactive");
                    errors.rejectValue("bookingId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                    return false;
                }
                optionalBookingVo = Optional.of(bookingVo);
            } catch (BookingException e) {
                log.debug("EngagementForm.bookingId is invalid");
                errors.rejectValue("bookingId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("EngagementForm.bookingId is valid");

        if(!fieldsToEscape.contains("tokenNumber") && form.getTokenNumber() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTokenNumber()))) {
            errors.rejectValue("tokenNumber", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("EngagementDto.tokenNumber is invalid");
            return false;
        }
        log.debug("EngagementForm.tokenNumber is valid");

        if(optionalBookingVo.isPresent()) {
            BookingVo bookingVo = optionalBookingVo.get();
            CategoryVo categoryVo = bookingVo.getCategory();
            switch (categoryVo.getName()) {
                case "Dine In":
                    if(!fieldsToEscape.contains("tableId") && form.getTableId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTableId()))) {
                        errors.rejectValue("tableId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                        log.debug("EngagementForm.tableId is invalid");
                        return false;
                    } else if(!fieldsToEscape.contains("tableId") && form.getTableId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getTableId()))) {
                        Errors err = new DirectFieldBindingResult(form.getTableId(), "EngagementForm");
                        tableIdValidator.validate(form.getTableId(), err);
                        if(err.hasErrors()) {
                            log.debug("EngagementForm.tableId is invalid");
                            CheckInErrorCode ec = CheckInErrorCode.valueOf(err.getGlobalError().getCode());
                            log.debug("EngagementForm error detail: {}", ec);
                            errors.rejectValue("tableId", ec.name());
                            return false;
                        }
                    }
                    log.debug("EngagementForm.tableId is valid");

                    if(!fieldsToEscape.contains("noOfPersons") && form.getNoOfPersons() != null && form.getNoOfPersons() <= 0) {
                        errors.rejectValue("noOfPersons", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                        log.debug("EngagementForm.noOfPersons is invalid");
                        return false;
                    }
                    log.debug("EngagementForm.noOfPersons is valid");
                    break;
                case "Take Away":
                    break;
                case "Delivery":
                    if(!fieldsToEscape.contains("extRef") && form.getExtRef() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getExtRef()))) {
                        errors.rejectValue("extRef", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                        log.debug("EngagementForm.extRef is invalid");
                        return false;
                    }
                    log.debug("EngagementForm.extRef is valid");
                    break;
                default:
                    errors.rejectValue("bookingId", CheckInErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED.name());
                    log.debug("EngagementForm.bookingId is invalid");
                    return false;
            }
        } else {
            errors.rejectValue("bookingId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("EngagementForm.bookingId is invalid");
            return false;
        }

        return true;
    }
}
