package com.teenthofabud.restaurant.solution.engagement.booking.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.engagement.booking.data.BookingDto;
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
public class BookingDtoValidator implements Validator {

    private List<String> fieldsToEscape;
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
        return clazz.isAssignableFrom(BookingDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        BookingDto dto = (BookingDto) target;

        /*Optional<String> optName = dto.getCategoryId();
        if(!fieldsToEscape.contains("name") && optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("BookingDto.name is invalid");
            return;
        }*/

        Optional<String> optTimestamp = dto.getTimestamp();
        if(!fieldsToEscape.contains("timestamp") && optTimestamp.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optTimestamp.get()))) {
            errors.rejectValue("timestamp", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("BookingDto.timestamp is invalid");
            return;
        } else if(!fieldsToEscape.contains("timestamp") && optTimestamp.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optTimestamp.get()))) {
            try {
                LocalDateTime dt = LocalDateTime.parse(optTimestamp.get(),DateTimeFormatter.ofPattern(bookingTimeFormat));
                if(dt.isBefore(LocalDateTime.now())) {
                    log.debug("BookingDto.timestamp is in past");
                    errors.rejectValue("timestamp", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (DateTimeParseException e) {
                log.debug("BookingDto.timestamp is invalid");
                errors.rejectValue("timestamp", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        /*Optional<String> optNoOfPerson = dto.getTimestamp();
        if(!fieldsToEscape.contains("noOfPerson") && optNoOfPerson.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optNoOfPerson.get()))) {
            errors.rejectValue("noOfPerson", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("BookingDto.noOfPerson is invalid");
            return;
        } else if(!fieldsToEscape.contains("noOfPerson") && optNoOfPerson.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optNoOfPerson.get()))) {
            try {
                Integer.parseInt(optNoOfPerson.get());
            } catch (NumberFormatException e) {
                log.debug("BookingDto.noOfPerson is invalid");
                errors.rejectValue("noOfPerson", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                return;
            }
        }*/

        Optional<String> optCategoryId = dto.getCategoryId();
        if(!fieldsToEscape.contains("categoryId") && optCategoryId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optCategoryId.get()))) {
            errors.rejectValue("categoryId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("BookingDto.categoryId is invalid");
            return;
        } else if(!fieldsToEscape.contains("categoryId") && optCategoryId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optCategoryId.get()))) {
            String categoryId = optCategoryId.get();
            /*try {
                Long.parseLong(categoryId);
            } catch (NumberFormatException e) {
                log.debug("BookingDto.categoryId is invalid");
                errors.rejectValue("categoryId", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                return;
            }*/
            try {
                CategoryVo categoryVo = categoryService.retrieveDetailsById(categoryId, Optional.of(TOABCascadeLevel.ONE));
                if(!categoryVo.getActive()) {
                    log.debug("BookingDto.categoryId is inactive");
                    errors.rejectValue("categoryId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (CategoryException e) {
                log.debug("BookingDto.categoryId is invalid");
                errors.rejectValue("categoryId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        /*Optional<String> optTableId = dto.getTableId();
        if(!fieldsToEscape.contains("tableId") && optTableId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optTableId.get()))) {
            errors.rejectValue("tableId", ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("BookingDto.tableId is invalid");
            return;
        } else if(!fieldsToEscape.contains("tableId") && optTableId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optTableId.get()))) {
            String tableId = optTableId.get();
            Errors err = new DirectFieldBindingResult(tableId, "BookingDto");
            tableIdValidator.validate(tableId, err);
            if(err.hasErrors()) {
                log.debug("BookingDto.tableId is invalid");
                ReservationErrorCode ec = ReservationErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("BookingDto error detail: {}", ec);
                errors.rejectValue("tableId", ec.name());
                return;
            }
        }*/

        Optional<String> optAccountId = dto.getAccountId();
        if(!fieldsToEscape.contains("accountId") && optAccountId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optAccountId.get()))) {
            errors.rejectValue("accountId", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("BookingDto.accountId is invalid");
            return;
        } else if(!fieldsToEscape.contains("accountId") && optAccountId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optAccountId.get()))) {
            String accountId = optAccountId.get();
            Errors err = new DirectFieldBindingResult(accountId, "BookingDto");
            accountIdValidator.validate(accountId, err);
            if(err.hasErrors()) {
                log.debug("BookingDto.accountId is invalid");
                CheckInErrorCode ec = CheckInErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("BookingDto error detail: {}", ec);
                errors.rejectValue("accountId", ec.name());
                return;
            }
        }

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
                log.debug("BookingDto.active is invalid");
                return;
            }
        }
    }

}
