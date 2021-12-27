package com.teenthofabud.restaurant.solution.settings.device.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceForm;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceType;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.io.IOException;
import java.util.List;

@Component
@Slf4j
public class DeviceFormRelaxedValidator implements RelaxedValidator<DeviceForm>  {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.device.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Boolean validateLoosely(DeviceForm form, Errors errors) {
        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("DeviceForm.name is empty");
            return false;
        }
        log.debug("DeviceForm.name is valid");
        if(!fieldsToEscape.contains("description") && form.getDescription() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            errors.rejectValue("description", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("DeviceForm.description is empty");
            return false;
        }
        log.debug("DeviceForm.description is valid");
        if(!fieldsToEscape.contains("deviceTypeId") && form.getDeviceTypeId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDeviceTypeId()))) {
            errors.rejectValue("deviceTypeId", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("DeviceForm.deviceTypeId is empty");
            return false;
        } else if(!fieldsToEscape.contains("deviceTypeId") && form.getDeviceTypeId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getDeviceTypeId()))) {
            try {
                DeviceType.valueOf(form.getDeviceTypeId());
            } catch (IllegalArgumentException e) {
                errors.rejectValue("deviceTypeId", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
                log.debug("DeviceForm.deviceTypeId is invalid");
                return false;
            }
        }
        log.debug("DeviceForm.deviceTypeId is valid");
        if(!fieldsToEscape.contains("location") && form.getLocation() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getLocation()))) {
            errors.rejectValue("location", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("DeviceForm.location is empty");
            return false;
        }
        log.debug("DeviceForm.content is valid");
        return true;
    }
}
