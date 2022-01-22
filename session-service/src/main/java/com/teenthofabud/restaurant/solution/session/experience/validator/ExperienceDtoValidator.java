package com.teenthofabud.restaurant.solution.session.experience.validator;

import com.teenthofabud.restaurant.solution.session.error.SessionErrorCode;
import com.teenthofabud.restaurant.solution.session.experience.data.ExperienceDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class ExperienceDtoValidator implements Validator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.session.experience.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(ExperienceDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        ExperienceDto dto = (ExperienceDto) target;
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            log.debug("ExperienceDto.name is invalid");
            return;
        }
        Optional<String> optDescription = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDescription.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optDescription.get()))) {
            errors.rejectValue("description", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            log.debug("ExperienceDto.description is invalid");
            return;
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
                log.debug("ExperienceDto.active is invalid");
                return;
            }
        }
    }

}
