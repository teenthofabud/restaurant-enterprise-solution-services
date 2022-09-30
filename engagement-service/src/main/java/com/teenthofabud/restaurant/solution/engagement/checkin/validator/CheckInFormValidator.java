package com.teenthofabud.restaurant.solution.engagement.checkin.validator;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInFormParameters;
import com.teenthofabud.restaurant.solution.engagement.error.EngagementErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;
import java.util.Optional;

@Slf4j
public abstract class CheckInFormValidator implements Validator {

    private List<String> fieldsToEscape;
    //private Validator tableIdValidator;
    private Validator accountIdValidator;
    //private CategoryService categoryService;

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

/*    @Autowired
    public void setCategoryService(CategoryService categoryService) {
        this.categoryService = categoryService;
    }*/
    
    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(CheckInForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        CheckInForm form = (CheckInForm) target;

        if(!fieldsToEscape.contains("notes") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getNotes()))) {
            log.debug("CheckInForm.notes is empty");
            errors.rejectValue("notes", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return;
        }
        log.debug("CheckInForm.notes is valid");

        /*if(!fieldsToEscape.contains("status") && form.getStatus() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getStatus()))) {
            errors.rejectValue("status", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInForm.status is invalid");
            return;
        } else if(!fieldsToEscape.contains("status") && form.getStatus() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getStatus()))) {
            try {
                CheckInStatus.valueOf(form.getStatus());
            } catch (IllegalArgumentException e) {
                log.debug("CheckInForm.status is invalid");
                errors.rejectValue("status", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("CheckInForm.status is valid");*/


        if(!fieldsToEscape.contains("noOfPersons") && (form.getNoOfPersons() != null || form.getNoOfPersons() <= 0)) {
            errors.rejectValue("noOfPersons", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInForm.noOfPersons is invalid");
            return;
        }
        log.debug("CheckInForm.noOfPersons is valid");

        if(!fieldsToEscape.contains("sequence") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getSequence()))) {
            errors.rejectValue("sequence", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInForm.sequence is invalid");
            return;
        }
        log.debug("CheckInForm.sequence is valid");

        /*if(!fieldsToEscape.contains("tableId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTableId()))) {
            log.debug("CheckInForm.tableId is empty");
            errors.rejectValue("tableId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("tableId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTableId()))){
            Errors err = new DirectFieldBindingResult(form.getTableId(), "CheckInForm");
            tableIdValidator.validate(form.getTableId(), err);
            if(err.hasErrors()) {
                log.debug("CheckInForm.tableId is invalid");
                EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("CheckInForm error detail: {}", ec);
                errors.rejectValue("tableId", ec.name());
                return;
            }
        }
        log.debug("CheckInForm.tableId is valid");*/

        if(!fieldsToEscape.contains("accountId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAccountId()))) {
            log.debug("CheckInForm.accountId is empty");
            errors.rejectValue("accountId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("accountId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))){
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

        this.validate(Optional.of(form.getAttributes()), errors);
    }

    protected abstract void validate(Optional<? extends CheckInFormParameters> optionalCheckInFormParameters, Errors errors);

}
