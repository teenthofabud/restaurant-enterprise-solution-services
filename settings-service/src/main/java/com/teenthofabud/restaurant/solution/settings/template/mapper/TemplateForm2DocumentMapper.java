package com.teenthofabud.restaurant.solution.settings.template.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.settings.template.data.TemplateDocument;
import com.teenthofabud.restaurant.solution.settings.template.data.TemplateForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class TemplateForm2DocumentMapper implements DualChannelMapper<TemplateDocument, TemplateForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.template.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<TemplateDocument> compareAndMap(TemplateDocument actualDocument, TemplateForm form) {
        TemplateDocument expectedDocument = new TemplateDocument();
        boolean changeSW = false;
        // direct copy
        expectedDocument.setId(actualDocument.getId());
        log.debug("Directly copying TemplateDocument.id: {} from actualDocument to expectedDocument", actualDocument.getId());
        expectedDocument.setCreatedOn(actualDocument.getCreatedOn());
        log.debug("Directly copying TemplateDocument.createdOn: {} from actualDocument to expectedDocument", actualDocument.getCreatedOn());
        expectedDocument.setActive(actualDocument.getActive());
        log.debug("Directly copying TemplateDocument.active: {} from actualDocument to expectedDocument", actualDocument.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName()))
                && form.getName().compareTo(actualDocument.getName()) != 0) {
            expectedDocument.setName(form.getName());
            changeSW = true;
            log.debug("TemplateForm.name: {} is different as TemplateDocument.name: {}", form.getName(), actualDocument.getName());
        } else {
            expectedDocument.setName(actualDocument.getName());
            log.debug("TemplateForm.name: is unchanged");
        }
        if(!fieldsToEscape.contains("description") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription()))
                && form.getDescription().compareTo(actualDocument.getDescription()) != 0) {
            expectedDocument.setDescription(form.getDescription());
            changeSW = true;
            log.debug("TemplateForm.description: {} is different as TemplateDocument.description: {}", form.getDescription(), actualDocument.getDescription());
        } else {
            expectedDocument.setDescription(actualDocument.getDescription());
            log.debug("TemplateForm.description: is unchanged");
        }
        if(!fieldsToEscape.contains("templateTypeId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTemplateTypeId()))
                && form.getTemplateTypeId().compareTo(actualDocument.getTemplateTypeId()) != 0) {
            expectedDocument.setTemplateTypeId(form.getTemplateTypeId());
            changeSW = true;
            log.debug("TemplateForm.templateTypeId: {} is different as TemplateDocument.templateTypeId: {}", form.getTemplateTypeId(), actualDocument.getTemplateTypeId());
        } else {
            expectedDocument.setTemplateTypeId(actualDocument.getTemplateTypeId());
            log.debug("TemplateForm.templateTypeId: is unchanged");
        }
        if(!fieldsToEscape.contains("content") && StringUtils.hasText(StringUtils.trimWhitespace(form.getContent()))
                && form.getContent().compareTo(actualDocument.getContent()) != 0) {
            expectedDocument.setContent(form.getContent());
            changeSW = true;
            log.debug("TemplateForm.content: {} is different as TemplateDocument.content: {}", form.getContent(), actualDocument.getContent());
        } else {
            expectedDocument.setContent(actualDocument.getContent());
            log.debug("TemplateForm.content: is unchanged");
        }
        return changeSW ? Optional.of(expectedDocument) : Optional.empty();
    }

}
