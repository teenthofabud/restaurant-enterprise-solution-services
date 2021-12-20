package com.teenthofabud.restaurant.solution.settings.discount.validator;

import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountForm;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;

@Component
@Slf4j
public class DiscountFormValidator implements Validator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.discount.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }
    
    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(DiscountForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        DiscountForm form = (DiscountForm) target;
        if(!fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("DiscountForm.name is empty");
            errors.rejectValue("name", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("rate") && (form.getRate() == null || form.getRate() < 0.0d)) {
            log.debug("DiscountForm.rate is invalid");
            errors.rejectValue("rate", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            return;
        }
    }

}
