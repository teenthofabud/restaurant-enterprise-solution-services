package com.teenthofabud.restaurant.solution.booking.association.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationDto;
import com.teenthofabud.restaurant.solution.booking.error.BookingErrorCode;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceException;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceVo;
import com.teenthofabud.restaurant.solution.booking.experience.service.ExperienceService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
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
public class AssociationDtoValidator implements Validator {

    private List<String> fieldsToEscape;
    private Validator tableIdValidator;
    private Validator accountIdValidator;
    private ExperienceService experienceService;
    //private String endedOnFormat;

    @Value("#{'${res.session.association.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    /*@Value("${res.booking.association.endednon.format}")
    public void setEndedOnFormat(String endedOnFormat) {
        this.endedOnFormat = endedOnFormat;
    }*/

    @Autowired
    @Qualifier("tableIdValidator")
    public void setTableIdValidator(Validator tableIdValidator) {
        this.tableIdValidator = tableIdValidator;
    }

    @Autowired
    @Qualifier("accountIdValidator")
    public void setAccountIdValidator(Validator accountIdValidator) {
        this.accountIdValidator = accountIdValidator;
    }

    @Autowired
    public void setExperienceService(ExperienceService experienceService) {
        this.experienceService = experienceService;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(AssociationDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        AssociationDto dto = (AssociationDto) target;

        Optional<String> optName = dto.getExperienceId();
        if(!fieldsToEscape.contains("name") && optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            log.debug("AssociationDto.name is invalid");
            return;
        }

        Optional<String> optExperienceId = dto.getExperienceId();
        if(!fieldsToEscape.contains("experienceId") && optExperienceId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optExperienceId.get()))) {
            errors.rejectValue("experienceId", BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            log.debug("AssociationDto.experienceId is invalid");
            return;
        } else if(!fieldsToEscape.contains("experienceId") && optExperienceId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optExperienceId.get()))) {
            String experienceId = optExperienceId.get();
            try {
                Long.parseLong(experienceId);
            } catch (NumberFormatException e) {
                log.debug("AssociationDto.experienceId is invalid");
                errors.rejectValue("experienceId", BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                return;
            }
            try {
                ExperienceVo experienceVo = experienceService.retrieveDetailsById(experienceId, Optional.of(TOABCascadeLevel.ONE));
                if(!experienceVo.getActive()) {
                    log.debug("AssociationDto.experienceId is inactive");
                    errors.rejectValue("experienceId", BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (ExperienceException e) {
                log.debug("AssociationDto.experienceId is invalid");
                errors.rejectValue("experienceId", BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<String> optTableId = dto.getTableId();
        if(!fieldsToEscape.contains("tableId") && optTableId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optTableId.get()))) {
            errors.rejectValue("tableId", BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            log.debug("AssociationDto.tableId is invalid");
            return;
        } else if(!fieldsToEscape.contains("tableId") && optTableId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optTableId.get()))) {
            String tableId = optTableId.get();
            Errors err = new DirectFieldBindingResult(tableId, "AssociationDto");
            tableIdValidator.validate(tableId, err);
            if(err.hasErrors()) {
                log.debug("AssociationDto.tableId is invalid");
                BookingErrorCode ec = BookingErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AssociationDto error detail: {}", ec);
                errors.rejectValue("tableId", ec.name());
                return;
            }
        }

        Optional<String> optAccountId = dto.getAccountId();
        if(!fieldsToEscape.contains("accountId") && optAccountId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optAccountId.get()))) {
            errors.rejectValue("accountId", BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            log.debug("AssociationDto.accountId is invalid");
            return;
        } else if(!fieldsToEscape.contains("accountId") && optAccountId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optAccountId.get()))) {
            String accountId = optAccountId.get();
            Errors err = new DirectFieldBindingResult(accountId, "AssociationDto");
            accountIdValidator.validate(accountId, err);
            if(err.hasErrors()) {
                log.debug("AssociationDto.accountId is invalid");
                BookingErrorCode ec = BookingErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AssociationDto error detail: {}", ec);
                errors.rejectValue("accountId", ec.name());
                return;
            }
        }

        /*Optional<String> optEndedOn = dto.getEndedOn();
        if(!fieldsToEscape.contains("endedOn") && optEndedOn.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optEndedOn.get()))) {
            errors.rejectValue("endedOn", SessionErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            log.debug("AssociationDto.endedOn is invalid");
            return;
        } else if(!fieldsToEscape.contains("endedOn") && optEndedOn.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optEndedOn.get()))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(endedOnFormat);
                LocalDate.parse(optEndedOn.get(), dtf);
            } catch (DateTimeParseException e) {
                errors.rejectValue("endedOn", SessionErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                log.debug("AssociationDto.endedOn is invalid");
                return;
            }
        }*/

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                log.debug("AssociationDto.active is invalid");
                return;
            }
        }
    }

}
