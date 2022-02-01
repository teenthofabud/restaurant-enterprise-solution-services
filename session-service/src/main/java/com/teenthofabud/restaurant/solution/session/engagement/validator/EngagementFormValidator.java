package com.teenthofabud.restaurant.solution.session.engagement.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.session.engagement.data.EngagementEvent;
import com.teenthofabud.restaurant.solution.session.engagement.data.EngagementForm;
import com.teenthofabud.restaurant.solution.session.error.SessionErrorCode;
import com.teenthofabud.restaurant.solution.session.association.data.AssociationException;
import com.teenthofabud.restaurant.solution.session.association.data.AssociationVo;
import com.teenthofabud.restaurant.solution.session.association.service.AssociationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class EngagementFormValidator implements Validator {

    private List<String> fieldsToEscape;
    private String dateFormat;
    private String timeFormat;
    private AssociationService associationService;

    @Value("#{'${res.session.association.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Value("${res.session.association.date.format}")
    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    @Value("${res.session.association.time.format}")
    public void setTimeFormat(String timeFormat) {
        this.timeFormat = timeFormat;
    }

    @Autowired
    public void setAssociationService(AssociationService associationService) {
        this.associationService = associationService;
    }
    
    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(EngagementForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        EngagementForm form = (EngagementForm) target;

        if(!fieldsToEscape.contains("associationId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAssociationId()))) {
            log.debug("EngagementForm.associationId is empty");
            errors.rejectValue("associationId", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("associationId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAssociationId()))){
            String associationId = form.getAssociationId();
            try {
                AssociationVo associationVo = associationService.retrieveDetailsById(associationId, Optional.of(TOABCascadeLevel.ONE));
                if(!associationVo.getActive()) {
                    log.debug("EngagementForm.associationId is inactive");
                    errors.rejectValue("associationId", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (AssociationException e) {
                log.debug("EngagementForm.associationId is invalid");
                errors.rejectValue("associationId", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("EngagementForm.associationId is valid");

        if(!fieldsToEscape.contains("event") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getEvent()))) {
            log.debug("EngagementForm.event is empty");
            errors.rejectValue("event", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("event") && StringUtils.hasText(StringUtils.trimWhitespace(form.getEvent()))){
            try {
                EngagementEvent.valueOf(form.getEvent());
            } catch (IllegalArgumentException e) {
                log.debug("EngagementDto.event is invalid");
                errors.rejectValue("event", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("EngagementForm.tableId is valid");

        if(!fieldsToEscape.contains("date") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDate()))) {
            errors.rejectValue("date", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            log.debug("EngagementForm.date is empty");
            return;
        } else if(!fieldsToEscape.contains("date") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDate()))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(dateFormat);
                LocalDate.parse(form.getDate(), dtf);
            } catch (DateTimeParseException e) {
                errors.rejectValue("date", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
                log.debug("EngagementForm.date is invalid");
                return;
            }
        }
        log.debug("EngagementForm.date is valid");

        if(!fieldsToEscape.contains("time") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTime()))) {
            errors.rejectValue("time", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            log.debug("EngagementForm.time is empty");
            return;
        } else if(!fieldsToEscape.contains("time") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTime()))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(dateFormat);
                LocalTime.parse(form.getTime(), dtf);
            } catch (DateTimeParseException e) {
                errors.rejectValue("time", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
                log.debug("EngagementForm.time is invalid");
                return;
            }
        }
        log.debug("EngagementForm.time is valid");

    }

}