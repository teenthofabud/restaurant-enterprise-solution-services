package com.teenthofabud.restaurant.solution.settings.discount.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountForm;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;

@Component
@Slf4j
public class DiscountFormRelaxedValidator implements RelaxedValidator<DiscountForm>  {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.discount.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Boolean validateLoosely(DiscountForm form, Errors errors) {
        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("DiscountForm.name is empty");
            return false;
        }
        log.debug("DiscountForm.name is valid");
        if(!fieldsToEscape.contains("description") && form.getDescription() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            errors.rejectValue("description", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("DiscountForm.description is empty");
            return false;
        }
        log.debug("DiscountForm.description is valid");
        if(!fieldsToEscape.contains("rate") && form.getRate() != null && form.getRate() < 0.0d) {
            errors.rejectValue("rate", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("DiscountForm.rate is invalid");
            return false;
        }
        log.debug("DiscountForm.rate is valid");
        return true;
    }
}
