package com.teenthofabud.restaurant.solution.settings.template.converter;

import com.teenthofabud.restaurant.solution.settings.template.data.TemplateDocument;
import com.teenthofabud.restaurant.solution.settings.template.data.TemplateForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class TemplateForm2DocumentConverter implements Converter<TemplateForm, TemplateDocument> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.template.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public TemplateDocument convert(TemplateForm form) {
        TemplateDocument entity = new TemplateDocument();
        if(!fieldsToEscape.contains("name")) {
            entity.setName(form.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            entity.setDescription(form.getDescription());
        }
        if(!fieldsToEscape.contains("templateTypeId")) {
            entity.setTemplateTypeId(form.getTemplateTypeId());
        }
        if(!fieldsToEscape.contains("content")) {
            entity.setContent(form.getContent());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
