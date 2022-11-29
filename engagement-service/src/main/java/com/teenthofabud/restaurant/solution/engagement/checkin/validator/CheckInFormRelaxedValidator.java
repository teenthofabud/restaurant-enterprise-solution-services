package com.teenthofabud.restaurant.solution.engagement.checkin.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
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
public abstract class CheckInFormRelaxedValidator implements RelaxedValidator<CheckInForm>  {

    private Validator accountIdValidator;

    public abstract List<String> getFieldsToEscape();

    @Autowired
    @Qualifier("accountIdValidator")
    public void setAccountIdValidator(Validator accountIdValidator) {
        this.accountIdValidator = accountIdValidator;
    }

    @Override
    public Boolean validateLoosely(CheckInForm form, Errors errors) {

        if(!getFieldsToEscape().contains("notes") && form.getNotes() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getNotes()))) {
            errors.rejectValue("notes", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInForm.notes is empty");
            return false;
        }
        log.debug("CheckInForm.notes is valid");

        /*if(!getFieldsToEscape().contains("status") && form.getStatus() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getStatus()))) {
            errors.rejectValue("status", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInForm.status is invalid");
            return false;
        } else if(!getFieldsToEscape().contains("status") && form.getStatus() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getStatus()))) {
            try {
                CheckInStatus.valueOf(form.getStatus());
            } catch (IllegalArgumentException e) {
                log.debug("CheckInForm.status is invalid");
                errors.rejectValue("status", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("CheckInForm.timestamp is valid");*/

        if(!getFieldsToEscape().contains("type") && form.getType() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getType()))) {
            errors.rejectValue("type", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInForm.type is invalid");
            return false;
        } else if(!getFieldsToEscape().contains("type") && form.getType() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getType()))) {
            try {
                CheckInType type = CheckInType.valueOf(form.getType());
                if(type.compareTo(getCheckInTypeInContext()) != 0) {
                    throw new IllegalArgumentException("CheckInType " + type + " not supported in child implementation");
                }
            } catch (IllegalArgumentException e) {
                log.debug("CheckInForm.type is invalid");
                errors.rejectValue("type", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("CheckInForm.type is valid");


        if(!getFieldsToEscape().contains("sequence") && form.getSequence() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getSequence()))) {
            errors.rejectValue("sequence", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("ReservationForm.sequence is invalid");
            return false;
        }
        log.debug("CheckInForm.sequence is valid");

        if(!getFieldsToEscape().contains("noOfPersons") && form.getNoOfPersons() != null && form.getNoOfPersons() <= 0) {
            errors.rejectValue("noOfPersons", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("ReservationForm.noOfPersons is invalid");
            return false;
        }
        log.debug("CheckInForm.noOfPersons is valid");

        if(!getFieldsToEscape().contains("accountId") && form.getAccountId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAccountId()))) {
            log.debug("CheckInForm.accountId is empty");
            errors.rejectValue("accountId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!getFieldsToEscape().contains("accountId") && form.getAccountId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))){
            Errors err = new DirectFieldBindingResult(form.getAccountId(), "ReservationForm");
            accountIdValidator.validate(form.getAccountId(), err);
            if(err.hasErrors()) {
                log.debug("CheckInForm.accountId is invalid");
                EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("CheckInForm error detail: {}", ec);
                errors.rejectValue("accountId", ec.name());
                return false;
            }
        }
        log.debug("CheckInForm.accountId is valid");

        Boolean childValidationResult = this.validateLoosely(Optional.of(form), errors);

        return childValidationResult;


    }

    public abstract Boolean validateLoosely(Optional<? extends CheckInForm> optionalCheckInFormParameters, Errors errors);

    protected abstract CheckInType getCheckInTypeInContext();
}
