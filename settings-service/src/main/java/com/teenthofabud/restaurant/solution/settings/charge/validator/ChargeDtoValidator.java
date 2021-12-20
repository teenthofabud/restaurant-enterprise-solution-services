package com.teenthofabud.restaurant.solution.settings.charge.validator;

import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeDto;
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
public class ChargeDtoValidator implements Validator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.charge.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(ChargeDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        ChargeDto dto = (ChargeDto) target;
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("ChargeDto.name is invalid");
            return;
        }
        Optional<String> optDescription = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDescription.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optDescription.get()))) {
            errors.rejectValue("description", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("ChargeDto.description is invalid");
            return;
        }
        Optional<String> optRate = dto.getRate();
        if(!fieldsToEscape.contains("rate") && optRate.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optRate.get()))) {
            errors.rejectValue("rate", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("ChargeDto.rate is empty");
            return;
        } else if(!fieldsToEscape.contains("rate") && optRate.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optRate.get()))) {
            try {
                if(Double.parseDouble(optRate.get()) < 0.0d) {
                    throw new NumberFormatException("charge rate can't be negative");
                }
            } catch (NumberFormatException e) {
                errors.rejectValue("rate", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
                log.debug("ChargeDto.rate is invalid");
                return;
            }
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
                log.debug("ChargeDto.active is invalid");
                return;
            }
        }
    }

}
