package com.teenthofabud.restaurant.solution.print.template.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateEntity;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class TemplateForm2EntityMapper implements DualChannelMapper<TemplateEntity, TemplateForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.print.template.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<TemplateEntity> compareAndMap(TemplateEntity actualEntity, TemplateForm form) {
        TemplateEntity expectedEntity = new TemplateEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying TemplateEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying TemplateEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying TemplateEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName()))
                && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("TemplateForm.name: {} is different as TemplateEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("TemplateForm.name: is unchanged");
        }
        if(!fieldsToEscape.contains("description") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription()))
                && form.getDescription().compareTo(actualEntity.getDescription()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("TemplateForm.description: {} is different as TemplateEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            log.debug("TemplateForm.description: is unchanged");
        }
        if(!fieldsToEscape.contains("templateTypeId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTemplateTypeId()))
                && form.getTemplateTypeId().compareTo(actualEntity.getTemplateTypeId()) != 0) {
            expectedEntity.setTemplateTypeId(form.getTemplateTypeId());
            changeSW = true;
            log.debug("TemplateForm.templateTypeId: {} is different as TemplateEntity.templateTypeId: {}", form.getTemplateTypeId(), actualEntity.getTemplateTypeId());
        } else {
            expectedEntity.setTemplateTypeId(actualEntity.getTemplateTypeId());
            log.debug("TemplateForm.templateTypeId: is unchanged");
        }
        if(!fieldsToEscape.contains("content") && StringUtils.hasText(StringUtils.trimWhitespace(form.getContent()))
                && form.getContent().compareTo(actualEntity.getContent()) != 0) {
            expectedEntity.setContent(form.getContent());
            changeSW = true;
            log.debug("TemplateForm.content: {} is different as TemplateEntity.content: {}", form.getContent(), actualEntity.getContent());
        } else {
            expectedEntity.setContent(actualEntity.getContent());
            log.debug("TemplateForm.content: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
