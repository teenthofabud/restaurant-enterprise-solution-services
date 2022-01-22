package com.teenthofabud.restaurant.solution.session.experience.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.session.error.SessionErrorCode;
import com.teenthofabud.restaurant.solution.session.experience.data.ExperienceForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;

@Component
@Slf4j
public class ExperienceFormRelaxedValidator implements RelaxedValidator<ExperienceForm>  {

    private List<String> fieldsToEscape;

    @Value("#{'${res.session.experience.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Boolean validateLoosely(ExperienceForm form, Errors errors) {
        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            log.debug("ExperienceForm.name is empty");
            return false;
        }
        log.debug("ExperienceForm.name is valid");
        if(!fieldsToEscape.contains("description") && form.getDescription() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            errors.rejectValue("description", SessionErrorCode.SESSION_ATTRIBUTE_INVALID.name());
            log.debug("ExperienceForm.description is empty");
            return false;
        }
        log.debug("ExperienceForm.description is valid");
        return true;
    }
}
