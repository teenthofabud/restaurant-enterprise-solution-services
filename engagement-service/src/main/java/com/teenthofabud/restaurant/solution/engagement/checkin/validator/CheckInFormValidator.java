package com.teenthofabud.restaurant.solution.engagement.checkin.validator;

import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;
import java.util.Optional;

@Slf4j
public abstract class CheckInFormValidator implements Validator {

    private Validator accountIdValidator;

    public abstract List<String> getFieldsToEscape();

    @Autowired
    @Qualifier("accountIdValidator")
    public void setAccountIdValidator(Validator accountIdValidator) {
        this.accountIdValidator = accountIdValidator;
    }
    
    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(CheckInForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        CheckInForm form = (CheckInForm) target;

        /*if(!getFieldsToEscape().contains("status") && form.getStatus() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getStatus()))) {
            errors.rejectValue("status", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInForm.status is invalid");
            return;
        } else if(!getFieldsToEscape().contains("status") && form.getStatus() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getStatus()))) {
            try {
                CheckInStatus.valueOf(form.getStatus());
            } catch (IllegalArgumentException e) {
                log.debug("CheckInForm.status is invalid");
                errors.rejectValue("status", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("CheckInForm.status is valid");*/

        if(!getFieldsToEscape().contains("type") && form.getType() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getType()))) {
            errors.rejectValue("type", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInForm.type is invalid");
            return;
        } else if(!getFieldsToEscape().contains("type") && form.getType() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getType()))) {
            try {
                CheckInType type = CheckInType.valueOf(form.getType());
                if(type.compareTo(getCheckInTypeInContext()) != 0) {
                    throw new IllegalArgumentException("CheckInType " + type + " not supported in child implementation");
                }
            } catch (IllegalArgumentException e) {
                log.debug("CheckInForm.type is invalid");
                errors.rejectValue("type", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("CheckInForm.type is valid");


        if(!getFieldsToEscape().contains("noOfPersons") && (form.getNoOfPersons() == null || form.getNoOfPersons() <= 0)) {
            errors.rejectValue("noOfPersons", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInForm.noOfPersons is invalid");
            return;
        }
        log.debug("CheckInForm.noOfPersons is valid");

        if(!getFieldsToEscape().contains("sequence") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getSequence()))) {
            errors.rejectValue("sequence", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInForm.sequence is invalid");
            return;
        }
        log.debug("CheckInForm.sequence is valid");

        if(!getFieldsToEscape().contains("accountId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAccountId()))) {
            log.debug("CheckInForm.accountId is empty");
            errors.rejectValue("accountId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return;
        } else if(!getFieldsToEscape().contains("accountId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))){
            Errors err = new DirectFieldBindingResult(form.getAccountId(), "CheckInForm");
            accountIdValidator.validate(form.getAccountId(), err);
            if(err.hasErrors()) {
                log.debug("CheckInForm.accountId is invalid");
                EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("CheckInForm error detail: {}", ec);
                errors.rejectValue("accountId", ec.name());
                return;
            }
        }
        log.debug("CheckInForm.accountId is valid");

        this.validate(Optional.of(form), errors);
    }

    protected abstract void validate(Optional<? extends CheckInForm> optionalCheckInFormParameters, Errors errors);

    protected abstract CheckInType getCheckInTypeInContext();

}
