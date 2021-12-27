package com.teenthofabud.restaurant.solution.settings.template.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
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

import java.io.IOException;
import java.util.List;

@Component
@Slf4j
public class TemplateFormRelaxedValidator implements RelaxedValidator<TemplateForm>  {

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
    public Boolean validateLoosely(TemplateForm form, Errors errors) {
        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("TemplateForm.name is empty");
            return false;
        }
        log.debug("TemplateForm.name is valid");
        if(!fieldsToEscape.contains("description") && form.getDescription() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            errors.rejectValue("description", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("TemplateForm.description is empty");
            return false;
        }
        log.debug("TemplateForm.description is valid");
        if(!fieldsToEscape.contains("templateTypeId") && form.getTemplateTypeId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTemplateTypeId()))) {
            errors.rejectValue("templateTypeId", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("TemplateForm.templateTypeId is empty");
            return false;
        } else if(!fieldsToEscape.contains("templateTypeId") && form.getTemplateTypeId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getTemplateTypeId()))) {
            try {
                TemplateType.valueOf(form.getTemplateTypeId());
            } catch (IllegalArgumentException e) {
                errors.rejectValue("templateTypeId", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
                log.debug("TemplateForm.templateTypeId is invalid");
                return false;
            }
        }
        log.debug("TemplateForm.templateTypeId is valid");
        if(!fieldsToEscape.contains("content") && form.getContent() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getContent()))) {
            errors.rejectValue("content", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
            log.debug("TemplateForm.content is empty");
            return false;
        } else if(!fieldsToEscape.contains("content") && form.getContent() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getContent()))) {
            try {
                new Template(freemarkerTemplateName, form.getContent(), freemarkerConfiguration);
            } catch (IOException e) {
                log.debug("TemplateForm.content is invalid");
                errors.rejectValue("content", SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("TemplateForm.content is valid");
        return true;
    }
}
