package com.teenthofabud.restaurant.solution.settings.device.validator;

import com.teenthofabud.restaurant.solution.settings.device.data.DeviceForm;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceType;
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
public class DeviceFormValidator implements Validator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.device.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }
    
    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(DeviceForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        DeviceForm form = (DeviceForm) target;
        if(!fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("DeviceForm.name is empty");
            errors.rejectValue("name", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("location") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getLocation()))) {
            log.debug("DeviceForm.location is empty");
            errors.rejectValue("location", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("deviceTypeId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDeviceTypeId()))) {
            log.debug("DeviceForm.deviceTypeId is empty");
            errors.rejectValue("deviceTypeId", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("deviceTypeId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDeviceTypeId()))) {
            try {
                DeviceType.valueOf(form.getDeviceTypeId());
            } catch (IllegalArgumentException e) {
                errors.rejectValue("deviceTypeId", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
                log.debug("DeviceForm.deviceTypeId is invalid");
                return;
            }
        }
    }

}
