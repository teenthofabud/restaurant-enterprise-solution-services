package com.teenthofabud.restaurant.solution.engagement.engagement.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.engagement.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.EngagementDto;
import com.teenthofabud.restaurant.solution.checkin.error.CheckInErrorCode;
import com.teenthofabud.restaurant.solution.engagement.booking.data.BookingException;
import com.teenthofabud.restaurant.solution.engagement.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.engagement.booking.service.BookingService;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.validator.TableIdValidator;
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
public class EngagementDtoValidator implements Validator {

    private List<String> fieldsToEscape;
    private BookingService bookingService;
    private TableIdValidator tableIdValidator;
/*    private String dateFormat;
    private String timeFormat;
    private String timestampFormat;*/

    @Value("#{'${res.reservation.engagement.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    /*@Value("${res.checkin.engagement.time.format}")
    public void setTimeFormat(String timeFormat) {
        this.timeFormat = timeFormat;
    }

    @Value("${res.checkin.engagement.date.format}")
    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    @Value("${res.checkin.engagement.timestamp.format}")
    public void setTimestampFormat(String timestampFormat) {
        this.timestampFormat = timestampFormat;
    }*/

    @Autowired
    public void setBookingService(BookingService bookingService) {
        this.bookingService = bookingService;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(EngagementDto.class);
    }

    @Autowired
    public void setTableIdValidator(TableIdValidator tableIdValidator) {
        this.tableIdValidator = tableIdValidator;
    }

    @Override
    public void validate(Object target, Errors errors) {
        EngagementDto dto = (EngagementDto) target;
        Optional<BookingVo> optionalBookingVo = Optional.empty();

        Optional<String> optBookingId = dto.getBookingId();
        if(!fieldsToEscape.contains("bookingId") && optBookingId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optBookingId.get()))) {
            errors.rejectValue("bookingId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("EngagementDto.bookingId is invalid");
            return;
        } else if(!fieldsToEscape.contains("bookingId") && optBookingId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optBookingId.get()))) {
            String bookingId = optBookingId.get();
            try {
                BookingVo bookingVo = bookingService.retrieveDetailsById(bookingId, Optional.of(TOABCascadeLevel.TWO));
                if(!bookingVo.getActive()) {
                    log.debug("EngagementDto.bookingId is inactive");
                    errors.rejectValue("bookingId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                    return;
                }
                optionalBookingVo = Optional.of(bookingVo);
            } catch (BookingException e) {
                log.debug("EngagementDto.bookingId is invalid");
                errors.rejectValue("bookingId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<String> optTokenNumber = dto.getTokenNumber();
        if(!fieldsToEscape.contains("tokenNumber") && optTokenNumber.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optTokenNumber.get()))) {
            errors.rejectValue("tokenNumber", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("EngagementDto.tokenNumber is invalid");
            return;
        }

        if(optionalBookingVo.isPresent()) {
            BookingVo bookingVo = optionalBookingVo.get();
            CategoryVo categoryVo = bookingVo.getCategory();
            switch (categoryVo.getName()) {
                case "Dine In":
                    Optional<String> optTableId = dto.getTableId();
                    if(!fieldsToEscape.contains("tableId") && optTableId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optTableId.get()))) {
                        errors.rejectValue("tableId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                        log.debug("EngagementDto.tableId is invalid");
                        return;
                    } else if(!fieldsToEscape.contains("tableId") && optTableId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optTableId.get()))) {
                        String tableId = optTableId.get();
                        Errors err = new DirectFieldBindingResult(tableId, "EngagementDto");
                        tableIdValidator.validate(tableId, err);
                        if(err.hasErrors()) {
                            log.debug("EngagementDto.tableId is invalid");
                            CheckInErrorCode ec = CheckInErrorCode.valueOf(err.getGlobalError().getCode());
                            log.debug("EngagementDto error detail: {}", ec);
                            errors.rejectValue("tableId", ec.name());
                            return;
                        }
                    }

                    Optional<String> optNoOfPersons = dto.getNoOfPersons();
                    if(!fieldsToEscape.contains("noOfPersons") && optNoOfPersons.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optNoOfPersons.get()))) {
                        errors.rejectValue("noOfPersons", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                        log.debug("EngagementDto.noOfPersons is invalid");
                        return;
                    } else if(!fieldsToEscape.contains("noOfPersons") && optNoOfPersons.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optNoOfPersons.get()))) {
                        try {
                            Integer noOfPersons = Integer.parseInt(optNoOfPersons.get());
                            if(noOfPersons <= 0) {
                                log.debug("EngagementDto.noOfPersons is invalid");
                                errors.rejectValue("noOfPersons", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                                return;
                            }
                        } catch (NumberFormatException e) {
                            log.debug("EngagementDto.noOfPersons is invalid");
                            errors.rejectValue("noOfPersons", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                            return;
                        }
                    }
                    break;
                case "Take Away":
                    break;
                case "Delivery":
                    Optional<String> optExtRef = dto.getExtRef();
                    if(!fieldsToEscape.contains("extRef") && optExtRef.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optExtRef.get()))) {
                        errors.rejectValue("extRef", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                        log.debug("EngagementDto.extRef is invalid");
                        return;
                    }
                    break;
                default:
                    errors.rejectValue("bookingId", CheckInErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED.name());
                    log.debug("EngagementDto.bookingId is invalid");
                    return;
            }
        } else {
            errors.rejectValue("bookingId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("EngagementDto.bookingId is invalid");
            return;
        }

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                log.debug("EngagementDto.active is invalid");
                return;
            }
        }
    }

}
