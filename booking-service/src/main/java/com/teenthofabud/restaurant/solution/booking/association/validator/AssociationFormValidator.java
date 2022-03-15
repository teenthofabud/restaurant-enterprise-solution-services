package com.teenthofabud.restaurant.solution.booking.association.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationForm;
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
public class AssociationFormValidator implements Validator {

    private List<String> fieldsToEscape;
    private String endedOnFormat;
    private Validator tableIdValidator;
    private Validator accountIdValidator;
    private ExperienceService experienceService;

    @Value("#{'${res.booking.association.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

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
        return clazz.isAssignableFrom(AssociationForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        AssociationForm form = (AssociationForm) target;

        if(!fieldsToEscape.contains("experienceId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getExperienceId()))) {
            log.debug("AssociationForm.experienceId is empty");
            errors.rejectValue("experienceId", BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("experienceId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getExperienceId()))){
            String experienceId = form.getExperienceId();
            try {
                ExperienceVo experienceVo = experienceService.retrieveDetailsById(experienceId, Optional.of(TOABCascadeLevel.ONE));
                if(!experienceVo.getActive()) {
                    log.debug("AssociationForm.experienceId is inactive");
                    errors.rejectValue("experienceId", BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (ExperienceException e) {
                log.debug("AssociationForm.experienceId is invalid");
                errors.rejectValue("experienceId", BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("AssociationForm.experienceId is valid");

        if(!fieldsToEscape.contains("tableId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTableId()))) {
            log.debug("AssociationForm.tableId is empty");
            errors.rejectValue("tableId", BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("tableId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTableId()))){
            Errors err = new DirectFieldBindingResult(form.getTableId(), "AssociationForm");
            tableIdValidator.validate(form.getTableId(), err);
            if(err.hasErrors()) {
                log.debug("AssociationForm.tableId is invalid");
                BookingErrorCode ec = BookingErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AssociationForm error detail: {}", ec);
                errors.rejectValue("tableId", ec.name());
                return;
            }
        }
        log.debug("AssociationForm.tableId is valid");

        if(!fieldsToEscape.contains("accountId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAccountId()))) {
            log.debug("AssociationForm.accountId is empty");
            errors.rejectValue("accountId", BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("accountId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))){
            Errors err = new DirectFieldBindingResult(form.getAccountId(), "AssociationForm");
            accountIdValidator.validate(form.getAccountId(), err);
            if(err.hasErrors()) {
                log.debug("AssociationForm.accountId is invalid");
                BookingErrorCode ec = BookingErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AssociationForm error detail: {}", ec);
                errors.rejectValue("accountId", ec.name());
                return;
            }
        }
        log.debug("AssociationForm.accountId is valid");
    }

}
