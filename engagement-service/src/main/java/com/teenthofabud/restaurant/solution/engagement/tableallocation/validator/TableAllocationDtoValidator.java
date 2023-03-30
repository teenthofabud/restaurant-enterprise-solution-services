package com.teenthofabud.restaurant.solution.engagement.tableallocation.validator;

import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationDto;
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

@Component
@Slf4j
public class TableAllocationDtoValidator extends TableAllocationValidator implements Validator {

    //@Autowired
    //@Qualifier("tableIdValidator")
    private Validator tableIdValidator;

    private List<String> fieldsToEscape;

    @Value("#{'${res.engagement.tableAllocation.fields-to-escape}'.split(',')}")
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
        return clazz.isAssignableFrom(TableAllocationDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        TableAllocationDto dto = (TableAllocationDto) target;

        Optional<String> optNotes = dto.getNotes();
        if(!fieldsToEscape.contains("notes") && optNotes.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optNotes.get()))) {
            errors.rejectValue("notes", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("TableAllocationDto.notes is invalid");
            return;
        }

        /*Optional<String> optStatus = dto.getStatus();
        if(!getFieldsToEscape().contains("status") && optStatus.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optStatus.get()))) {
            errors.rejectValue("status", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("TableAllocationDto.status is invalid");
            return;
        } else if(!getFieldsToEscape().contains("status") && optStatus.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optStatus.get()))) {
            String status = optStatus.get();
            try {
                TableAllocationStatus.valueOf(status);
            } catch (IllegalArgumentException e) {
                log.debug("TableAllocationDto.status is invalid");
                errors.rejectValue("status", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                return;
            }
        }*/

        Optional<String> optTableId = dto.getTableId();
        if(!fieldsToEscape.contains("tableId") && optTableId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optTableId.get()))) {
            errors.rejectValue("tableId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            log.debug("TableAllocationDto.tableId is invalid");
            return;
        } else if(!fieldsToEscape.contains("tableId") && optTableId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optTableId.get()))) {
            String tableId = optTableId.get();
            Errors err = new DirectFieldBindingResult(tableId, "TableAllocationDto");
            tableIdValidator.validate(tableId, err);
            if(err.hasErrors()) {
                log.debug("TableAllocationDto.tableId is invalid");
                EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("TableAllocationDto error detail: {}", ec);
                errors.rejectValue("tableId", ec.name());
                return;
            }
        }

        Optional<String> optCheckInId = dto.getCheckInId();
        if(!fieldsToEscape.contains("checkInId") && optCheckInId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optCheckInId.get()))) {
            log.debug("TableAllocationDto.checkInId is empty");
            errors.rejectValue("checkInId", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("checkInId") && optCheckInId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optCheckInId.get()))){
            boolean flag = super.validateCheckInId(optCheckInId.get(), errors);
            if(!flag) {
                return;
            }
        }

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.name());
                log.debug("TableAllocationDto.active is invalid");
                return;
            }
        }
    }
}
