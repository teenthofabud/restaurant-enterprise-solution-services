package com.teenthofabud.restaurant.solution.encounter.meeting.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
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
public abstract class MeetingFormRelaxedValidator implements RelaxedValidator<MeetingForm>  {


    private Validator accountIdValidator;

    public abstract List<String> getFieldsToEscape();

    @Autowired
    @Qualifier("accountIdValidator")
    public void setAccountIdValidator(Validator accountIdValidator) {
        this.accountIdValidator = accountIdValidator;
    }

    @Override
    public Boolean validateLoosely(MeetingForm form, Errors errors) {

        if(!getFieldsToEscape().contains("sequence") && form.getSequence() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getSequence()))) {
            errors.rejectValue("sequence", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            log.debug("MeetingForm.sequence is invalid");
            return false;
        }
        log.debug("MeetingForm.sequence is valid");

        if(!getFieldsToEscape().contains("accountId") && form.getAccountId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAccountId()))) {
            log.debug("MeetingForm.accountId is empty");
            errors.rejectValue("accountId", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!getFieldsToEscape().contains("accountId") && form.getAccountId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))){
            Errors err = new DirectFieldBindingResult(form.getAccountId(), "ReservationForm");
            accountIdValidator.validate(form.getAccountId(), err);
            if(err.hasErrors()) {
                log.debug("MeetingForm.accountId is invalid");
                EncounterErrorCode ec = EncounterErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("MeetingForm error detail: {}", ec);
                errors.rejectValue("accountId", ec.name());
                return false;
            }
        }
        log.debug("MeetingForm.accountId is valid");

        Boolean childValidationResult = this.validateLoosely(Optional.of(form), errors);

        return childValidationResult;


    }

    public abstract Boolean validateLoosely(Optional<? extends MeetingForm> optionalMeetingFormParameters, Errors errors);
}
