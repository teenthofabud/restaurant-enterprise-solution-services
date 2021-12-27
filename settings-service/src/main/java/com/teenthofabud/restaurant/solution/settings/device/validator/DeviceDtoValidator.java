package com.teenthofabud.restaurant.solution.settings.device.validator;

import com.teenthofabud.restaurant.solution.settings.device.data.DeviceDto;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceType;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class DeviceDtoValidator implements Validator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.device.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(DeviceDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        DeviceDto dto = (DeviceDto) target;
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("DeviceDto.name is invalid");
            return;
        }
        Optional<String> optDescription = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDescription.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optDescription.get()))) {
            errors.rejectValue("description", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("DeviceDto.description is invalid");
            return;
        }
        Optional<String> optDeviceTypeId = dto.getDeviceTypeId();
        if(!fieldsToEscape.contains("deviceTypeId") && optDeviceTypeId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optDeviceTypeId.get()))) {
            errors.rejectValue("deviceTypeId", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("DeviceDto.deviceTypeId is empty");
            return;
        } else if(!fieldsToEscape.contains("deviceTypeId") && optDeviceTypeId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optDeviceTypeId.get()))) {
            try {
                DeviceType.valueOf(optDeviceTypeId.get());
            } catch (IllegalArgumentException e) {
                errors.rejectValue("deviceTypeId", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
                log.debug("DeviceDto.deviceTypeId is invalid");
                return;
            }
        }
        Optional<String> optLocation = dto.getLocation();
        if(!fieldsToEscape.contains("location") && optLocation.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optLocation.get()))) {
            errors.rejectValue("location", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("DeviceDto.location is empty");
            return;
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
                log.debug("DeviceDto.active is invalid");
                return;
            }
        }
    }

}
