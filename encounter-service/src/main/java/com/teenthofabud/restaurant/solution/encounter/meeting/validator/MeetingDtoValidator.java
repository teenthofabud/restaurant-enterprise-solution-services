package com.teenthofabud.restaurant.solution.encounter.meeting.validator;

import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingDto;
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
public abstract class MeetingDtoValidator implements Validator {

    public abstract List<String> getFieldsToEscape();
    //private Validator tableIdValidator;
    private Validator accountIdValidator;

    @Autowired
    @Qualifier("accountIdValidator")
    public void setAccountIdValidator(Validator accountIdValidator) {
        this.accountIdValidator = accountIdValidator;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(MeetingDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        MeetingDto dto = (MeetingDto) target;

        Optional<String> optSequence = dto.getSequence();
        if(optSequence.isPresent() && !getFieldsToEscape().contains("sequence") && StringUtils.isEmpty(StringUtils.trimWhitespace(optSequence.get()))) {
            errors.rejectValue("sequence", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            log.debug("MeetingDto.sequence is invalid");
            return;
        }

        Optional<String> optAccountId = dto.getAccountId();
        if(optAccountId.isPresent() && !getFieldsToEscape().contains("accountId") && StringUtils.isEmpty(StringUtils.trimWhitespace(optAccountId.get()))) {
            errors.rejectValue("accountId", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
            log.debug("MeetingDto.accountId is invalid");
            return;
        } else if(!getFieldsToEscape().contains("accountId") && optAccountId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optAccountId.get()))) {
            String accountId = optAccountId.get();
            Errors err = new DirectFieldBindingResult(accountId, "MeetingDto");
            accountIdValidator.validate(accountId, err);
            if(err.hasErrors()) {
                log.debug("MeetingDto.accountId is invalid");
                EncounterErrorCode ec = EncounterErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("MeetingDto error detail: {}", ec);
                errors.rejectValue("accountId", ec.name());
                return;
            }
        }

        Optional<String> optActive = dto.getActive();
        if(optActive.isPresent() && !getFieldsToEscape().contains("active") && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID.name());
                log.debug("MeetingDto.active is invalid");
                return;
            }
        }

        this.validate(Optional.of(dto), errors);
    }

    protected abstract void validate(Optional<? extends MeetingDto> optionalMeetingDtoParameters, Errors errors);

}
