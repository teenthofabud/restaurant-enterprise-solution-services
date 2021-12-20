package com.teenthofabud.restaurant.solution.settings.charge.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeForm;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;

@Component
@Slf4j
public class ChargeFormRelaxedValidator implements RelaxedValidator<ChargeForm>  {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.charge.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Boolean validateLoosely(ChargeForm form, Errors errors) {
        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("ChargeForm.name is empty");
            return false;
        }
        log.debug("ChargeForm.name is valid");
        if(!fieldsToEscape.contains("description") && form.getDescription() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            errors.rejectValue("description", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("ChargeForm.description is empty");
            return false;
        }
        log.debug("ChargeForm.description is valid");
        if(!fieldsToEscape.contains("rate") && form.getRate() != null && form.getRate() < 0.0d) {
            errors.rejectValue("rate", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("ChargeForm.rate is invalid");
            return false;
        }
        log.debug("ChargeForm.rate is valid");
        return true;
    }
}
