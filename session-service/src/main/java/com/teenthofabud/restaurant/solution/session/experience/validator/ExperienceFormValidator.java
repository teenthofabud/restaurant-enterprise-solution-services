package com.teenthofabud.restaurant.solution.session.experience.validator;

import com.teenthofabud.restaurant.solution.session.error.SessionErrorCode;
import com.teenthofabud.restaurant.solution.session.experience.data.ExperienceForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;

@Component
@Slf4j
public class ExperienceFormValidator implements Validator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.session.experience.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }
    
    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(ExperienceForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        ExperienceForm form = (ExperienceForm) target;
        if(!fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("ExperienceForm.name is empty");
            errors.rejectValue("name", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            return;
        }
    }

}
