package com.teenthofabud.restaurant.solution.engagement.tableallocation.validator;

import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationForm;
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

@Component
@Slf4j
public class TableAllocationFormValidator extends TableAllocationValidator implements Validator {

    //@Autowired
    //@Qualifier("tableIdValidator")
    private Validator tableIdValidator;

    @Value("#{'${res.engagement.tableAllocation.fields-to-escape}'.split(',')}")
    private List<String> fieldsToEscape;

    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    @Qualifier("tableIdValidator")
    public void setTableIdValidator(Validator tableIdValidator) {
        this.tableIdValidator = tableIdValidator;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(TableAllocationForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        TableAllocationForm form = (TableAllocationForm) target;

        if(!fieldsToEscape.contains("notes") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getNotes()))) {
            log.debug("TableAllocationForm.notes is empty");
            errors.rejectValue("notes", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return;
        }
        log.debug("TableAllocationForm.notes is valid");

        /*if(!getFieldsToEscape().contains("status") && form.getStatus() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getStatus()))) {
            errors.rejectValue("status", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("TableAllocationForm.status is invalid");
            return;
        } else if(!getFieldsToEscape().contains("status") && form.getStatus() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getStatus()))) {
            try {
                TableAllocationStatus.valueOf(form.getStatus());
            } catch (IllegalArgumentException e) {
                log.debug("TableAllocationForm.status is invalid");
                errors.rejectValue("status", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("TableAllocationForm.status is valid");*/


        if(!fieldsToEscape.contains("tableId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTableId()))) {
            log.debug("TableAllocationForm.tableId is empty");
            errors.rejectValue("tableId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("tableId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTableId()))){
            Errors err = new DirectFieldBindingResult(form.getTableId(), "TableAllocationForm");
            tableIdValidator.validate(form.getTableId(), err);
            if(err.hasErrors()) {
                log.debug("TableAllocationForm.tableId is invalid");
                EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("TableAllocationForm error detail: {}", ec);
                errors.rejectValue("tableId", ec.name());
                return;
            }
        }
        log.debug("TableAllocationForm.tableId is valid");

        if(!fieldsToEscape.contains("checkInId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCheckInId()))) {
            log.debug("TableAllocationForm.checkInId is empty");
            errors.rejectValue("checkInId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("checkInId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCheckInId()))){
            boolean flag = super.validateCheckInId(form.getCheckInId(), errors);
            if(!flag) {
                return;
            }
        }
        log.debug("TableAllocationForm.checkInId is valid");
    }

}
