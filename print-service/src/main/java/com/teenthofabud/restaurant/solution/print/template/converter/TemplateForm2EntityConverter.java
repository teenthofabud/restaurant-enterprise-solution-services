package com.teenthofabud.restaurant.solution.print.template.converter;

import com.teenthofabud.restaurant.solution.print.template.data.TemplateEntity;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class TemplateForm2EntityConverter implements Converter<TemplateForm, TemplateEntity> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.print.template.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public TemplateEntity convert(TemplateForm form) {
        TemplateEntity entity = new TemplateEntity();
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
