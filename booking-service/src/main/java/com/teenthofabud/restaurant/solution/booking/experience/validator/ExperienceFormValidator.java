package com.teenthofabud.restaurant.solution.booking.experience.validator;

import com.teenthofabud.restaurant.solution.booking.error.BookingErrorCode;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceForm;
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

    @Value("#{'${res.booking.experience.fields-to-escape}'.split(',')}")
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
            errors.rejectValue("name", BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.name());
            return;
        }
    }

}
