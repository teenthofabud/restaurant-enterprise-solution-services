package com.teenthofabud.restaurant.solution.settings.paymentmethod.validator;

import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;

@Component
@Slf4j
public class PaymentMethodFormValidator implements Validator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.paymentmethod.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }
    
    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(PaymentMethodForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        PaymentMethodForm form = (PaymentMethodForm) target;
        if(!fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("PaymentMethodForm.name is empty");
            errors.rejectValue("name", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            return;
        }
    }

}
