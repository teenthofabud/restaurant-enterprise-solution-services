package com.teenthofabud.restaurant.solution.settings.template.validator;

import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import com.teenthofabud.restaurant.solution.settings.template.data.TemplateDto;
import com.teenthofabud.restaurant.solution.settings.template.data.TemplateType;
import freemarker.template.Configuration;
import freemarker.template.Template;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.io.IOException;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class TemplateDtoValidator implements Validator {

    private List<String> fieldsToEscape;
    private Configuration freemarkerConfiguration;
    private String freemarkerTemplateName;

    @Autowired
    public void setFreemarkerConfiguration(Configuration freemarkerConfiguration) {
        this.freemarkerConfiguration = freemarkerConfiguration;
    }

    @Value("${res.settings.template.path}")
    public void setFreemarkerTemplateName(String freemarkerTemplateName) {
        this.freemarkerTemplateName = freemarkerTemplateName;
    }

    @Value("#{'${res.settings.template.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(TemplateDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        TemplateDto dto = (TemplateDto) target;
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("TemplateDto.name is invalid");
            return;
        }
        Optional<String> optDescription = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDescription.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optDescription.get()))) {
            errors.rejectValue("description", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("TemplateDto.description is invalid");
            return;
        }
        Optional<String> optTemplateTypeId = dto.getTemplateTypeId();
        if(!fieldsToEscape.contains("templateTypeId") && optTemplateTypeId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optTemplateTypeId.get()))) {
            errors.rejectValue("templateTypeId", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("TemplateDto.templateTypeId is empty");
            return;
        } else if(!fieldsToEscape.contains("templateTypeId") && optTemplateTypeId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optTemplateTypeId.get()))) {
            try {
                TemplateType.valueOf(optTemplateTypeId.get());
            } catch (IllegalArgumentException e) {
                errors.rejectValue("templateTypeId", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
                log.debug("TemplateDto.templateTypeId is invalid");
                return;
            }
        }
        Optional<String> optContent = dto.getContent();
        if(!fieldsToEscape.contains("content") && optContent.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optContent.get()))) {
            errors.rejectValue("content", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("TemplateDto.content is empty");
            return;
        } else if(!fieldsToEscape.contains("content") && optContent.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optContent.get()))) {
            try {
                new Template(freemarkerTemplateName, optContent.get(), freemarkerConfiguration);
            } catch (IOException e) {
                log.debug("TemplateForm.content is invalid");
                errors.rejectValue("content", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
                log.debug("TemplateDto.active is invalid");
                return;
            }
        }
    }

}
