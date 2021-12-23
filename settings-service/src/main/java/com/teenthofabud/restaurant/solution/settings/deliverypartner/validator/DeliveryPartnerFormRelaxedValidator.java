package com.teenthofabud.restaurant.solution.settings.deliverypartner.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerForm;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;

@Component
@Slf4j
public class DeliveryPartnerFormRelaxedValidator implements RelaxedValidator<DeliveryPartnerForm>  {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.deliverypartner.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Boolean validateLoosely(DeliveryPartnerForm form, Errors errors) {
        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("DeliveryPartnerForm.name is empty");
            return false;
        }
        log.debug("DeliveryPartnerForm.name is valid");
        if(!fieldsToEscape.contains("description") && form.getDescription() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            errors.rejectValue("description", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("DeliveryPartnerForm.description is empty");
            return false;
        }
        log.debug("DeliveryPartnerForm.description is valid");
        return true;
    }
}
