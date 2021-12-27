package com.teenthofabud.restaurant.solution.settings.template.validator;

import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import com.teenthofabud.restaurant.solution.settings.template.data.TemplateForm;
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

@Component
@Slf4j
public class TemplateFormValidator implements Validator {

    private List<String> fieldsToEscape;
    private String freemarkerTemplateName;
    private Configuration freemarkerConfiguration;

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
        return clazz.isAssignableFrom(TemplateForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        TemplateForm form = (TemplateForm) target;
        if(!fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("TemplateForm.name is empty");
            errors.rejectValue("name", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("content") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getContent()))) {
            log.debug("TemplateForm.content is empty");
            errors.rejectValue("content", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("content") && StringUtils.hasText(StringUtils.trimWhitespace(form.getContent()))) {
            try {
                new Template(freemarkerTemplateName, form.getContent(), freemarkerConfiguration);
            } catch (IOException e) {
                log.debug("TemplateForm.content is invalid");
                errors.rejectValue("content", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        if(!fieldsToEscape.contains("templateTypeId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTemplateTypeId()))) {
            log.debug("TemplateForm.templateTypeId is empty");
            errors.rejectValue("templateTypeId", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("templateTypeId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTemplateTypeId()))) {
            try {
                TemplateType.valueOf(form.getTemplateTypeId());
            } catch (IllegalArgumentException e) {
                errors.rejectValue("templateTypeId", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
                log.debug("TemplateForm.templateTypeId is invalid");
                return;
            }
        }
    }

}
