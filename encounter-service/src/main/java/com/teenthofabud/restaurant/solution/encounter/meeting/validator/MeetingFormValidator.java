package com.teenthofabud.restaurant.solution.encounter.meeting.validator;

import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import constants.EncounterErrorCode;
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
public abstract class MeetingFormValidator implements Validator {

    private List<String> fieldsToEscape;
    private Validator accountIdValidator;

    @Value("#{'${res.encounter.checkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    @Qualifier("accountIdValidator")
    public void setAccountIdValidator(Validator accountIdValidator) {
        this.accountIdValidator = accountIdValidator;
    }
    
    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(MeetingForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        MeetingForm form = (MeetingForm) target;

        if(!fieldsToEscape.contains("sequence") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getSequence()))) {
            errors.rejectValue("sequence", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            log.debug("MeetingForm.sequence is invalid");
            return;
        }
        log.debug("MeetingForm.sequence is valid");

        if(!fieldsToEscape.contains("accountId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAccountId()))) {
            log.debug("MeetingForm.accountId is empty");
            errors.rejectValue("accountId", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("accountId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))){
            Errors err = new DirectFieldBindingResult(form.getAccountId(), "MeetingForm");
            accountIdValidator.validate(form.getAccountId(), err);
            if(err.hasErrors()) {
                log.debug("MeetingForm.accountId is invalid");
                EncounterErrorCode ec = EncounterErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("MeetingForm error detail: {}", ec);
                errors.rejectValue("accountId", ec.name());
                return;
            }
        }
        log.debug("MeetingForm.accountId is valid");

        this.validate(Optional.of(form), errors);
    }

    protected abstract void validate(Optional<? extends MeetingForm> optionalMeetingFormParameters, Errors errors);

}
