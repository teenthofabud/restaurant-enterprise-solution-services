package com.teenthofabud.restaurant.solution.engagement.checkin.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.engagement.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInFormDetails;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInStatus;
import com.teenthofabud.restaurant.solution.engagement.error.EngagementErrorCode;
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

@Slf4j
public abstract class CheckInFormRelaxedValidator implements RelaxedValidator<CheckInForm>  {

    private List<String> fieldsToEscape;
    //private Validator tableIdValidator;
    private Validator accountIdValidator;

    @Value("#{'${res.engagement.checkIn.fields-to-escape}'.split(',')}")
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

    @Override
    public Boolean validateLoosely(CheckInForm form, Errors errors) {

        if(!fieldsToEscape.contains("notes") && form.getNotes() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getNotes()))) {
            errors.rejectValue("notes", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInForm.notes is empty");
            return false;
        }
        log.debug("CheckInForm.notes is valid");

        /*if(!fieldsToEscape.contains("status") && form.getStatus() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getStatus()))) {
            errors.rejectValue("status", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInForm.status is invalid");
            return false;
        } else if(!fieldsToEscape.contains("status") && form.getStatus() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getStatus()))) {
            try {
                CheckInStatus.valueOf(form.getStatus());
            } catch (IllegalArgumentException e) {
                log.debug("CheckInForm.status is invalid");
                errors.rejectValue("status", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("CheckInForm.timestamp is valid");*/


        if(!fieldsToEscape.contains("sequence") && form.getSequence() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getSequence()))) {
            errors.rejectValue("sequence", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("ReservationForm.sequence is invalid");
            return false;
        }
        log.debug("CheckInForm.sequence is valid");

        if(!fieldsToEscape.contains("noOfPersons") && form.getNoOfPersons() != null && form.getNoOfPersons() <= 0) {
            errors.rejectValue("noOfPersons", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("ReservationForm.noOfPersons is invalid");
            return false;
        }
        log.debug("CheckInForm.noOfPersons is valid");

        /*if(!fieldsToEscape.contains("tableId") && form.getTableId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTableId()))) {
            log.debug("ReservationForm.tableId is empty");
            errors.rejectValue("tableId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("tableId") && form.getTableId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getTableId()))){
            Errors err = new DirectFieldBindingResult(form.getTableId(), "ReservationForm");
            tableIdValidator.validate(form.getTableId(), err);
            if(err.hasErrors()) {
                log.debug("ReservationForm.tableId is invalid");
                EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("ReservationForm error detail: {}", ec);
                errors.rejectValue("tableId", ec.name());
                return false;
            }
        }
        log.debug("ReservationForm.tableId is valid");*/

        if(!fieldsToEscape.contains("accountId") && form.getAccountId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAccountId()))) {
            log.debug("CheckInForm.accountId is empty");
            errors.rejectValue("accountId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("accountId") && form.getAccountId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))){
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

        return true;


    }

    public abstract Boolean validateLoosely(Optional<? extends CheckInFormDetails> optionalCheckInFormDetails, Errors errors);
}
