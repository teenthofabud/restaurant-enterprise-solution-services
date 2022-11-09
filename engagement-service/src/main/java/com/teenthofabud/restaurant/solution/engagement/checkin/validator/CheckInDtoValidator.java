package com.teenthofabud.restaurant.solution.engagement.checkin.validator;

import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInDto;
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
public abstract class CheckInDtoValidator implements Validator {

    private Validator accountIdValidator;

    public abstract List<String> getFieldsToEscape();

    @Autowired
    @Qualifier("accountIdValidator")
    public void setAccountIdValidator(Validator accountIdValidator) {
        this.accountIdValidator = accountIdValidator;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(CheckInDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        CheckInDto dto = (CheckInDto) target;

        Optional<String> optNotes = dto.getNotes();
        if(!getFieldsToEscape().contains("notes") && optNotes.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optNotes.get()))) {
            errors.rejectValue("notes", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInDto.notes is invalid");
            return;
        }

        Optional<String> optSequence = dto.getSequence();
        if(!getFieldsToEscape().contains("sequence") && optSequence.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optSequence.get()))) {
            errors.rejectValue("sequence", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInDto.sequence is invalid");
            return;
        }

        Optional<Integer> optNoOfPersons = dto.getNoOfPersons();
        if(!getFieldsToEscape().contains("noOfPersons") && optNoOfPersons.isPresent() && optNoOfPersons.get() <= 0) {
            errors.rejectValue("noOfPersons", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInDto.noOfPersons is invalid");
            return;
        }

        /*Optional<String> optStatus = dto.getStatus();
        if(!getFieldsToEscape().contains("status") && optStatus.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optStatus.get()))) {
            errors.rejectValue("status", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInDto.status is invalid");
            return;
        } else if(!getFieldsToEscape().contains("status") && optStatus.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optStatus.get()))) {
            String status = optStatus.get();
            try {
                CheckInStatus.valueOf(status);
            } catch (IllegalArgumentException e) {
                log.debug("CheckInDto.status is invalid");
                errors.rejectValue("status", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                return;
            }
        }*/

        /*Optional<String> optTableId = dto.getTableId();
        if(!getFieldsToEscape().contains("tableId") && optTableId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optTableId.get()))) {
            errors.rejectValue("tableId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInDto.tableId is invalid");
            return;
        } else if(!getFieldsToEscape().contains("tableId") && optTableId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optTableId.get()))) {
            String tableId = optTableId.get();
            Errors err = new DirectFieldBindingResult(tableId, "CheckInDto");
            tableIdValidator.validate(tableId, err);
            if(err.hasErrors()) {
                log.debug("CheckInDto.tableId is invalid");
                EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("CheckInDto error detail: {}", ec);
                errors.rejectValue("tableId", ec.name());
                return;
            }
        }*/

        Optional<String> optType = dto.getType();
        if(!getFieldsToEscape().contains("type") && optType.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optType.get()))) {
            errors.rejectValue("type", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInDto.type is invalid");
            return;
        } else if(!getFieldsToEscape().contains("type") && optType.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optType.get()))) {
            try {
                CheckInType type = CheckInType.valueOf(optType.get());
                if(type.compareTo(getCheckInTypeInContext()) != 0) {
                    throw new IllegalArgumentException("CheckInType " + type + " not supported in child implementation");
                }
            } catch (IllegalArgumentException e) {
                errors.rejectValue("type", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                log.debug("CheckInDto.type is invalid");
                return;
            }
        }

        Optional<String> optAccountId = dto.getAccountId();
        if(!getFieldsToEscape().contains("accountId") && optAccountId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optAccountId.get()))) {
            errors.rejectValue("accountId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("CheckInDto.accountId is invalid");
            return;
        } else if(!getFieldsToEscape().contains("accountId") && optAccountId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optAccountId.get()))) {
            String accountId = optAccountId.get();
            Errors err = new DirectFieldBindingResult(accountId, "CheckInDto");
            accountIdValidator.validate(accountId, err);
            if(err.hasErrors()) {
                log.debug("CheckInDto.accountId is invalid");
                EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("CheckInDto error detail: {}", ec);
                errors.rejectValue("accountId", ec.name());
                return;
            }
        }

        Optional<String> optActive = dto.getActive();
        if(!getFieldsToEscape().contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                log.debug("CheckInDto.active is invalid");
                return;
            }
        }

        this.validate(Optional.of(dto), errors);
    }

    protected abstract void validate(Optional<? extends CheckInDto> optionalCheckInDtoParameters, Errors errors);

    protected abstract CheckInType getCheckInTypeInContext();

}
