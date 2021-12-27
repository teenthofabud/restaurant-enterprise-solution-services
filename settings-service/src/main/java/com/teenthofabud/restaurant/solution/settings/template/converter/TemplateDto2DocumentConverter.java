package com.teenthofabud.restaurant.solution.settings.template.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.settings.template.data.TemplateDto;
import com.teenthofabud.restaurant.solution.settings.template.data.TemplateDocument;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class TemplateDto2DocumentConverter implements ComparativePatchConverter<TemplateDto, TemplateDocument> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 5;

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.template.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public void compareAndMap(TemplateDto dto, TemplateDocument actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent()) {
            actualEntity.setName(optName.get());
            changeSW[i++] = true;
            log.debug("TemplateDto.name is valid");
        }
        Optional<String> optDescription = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDescription.isPresent()) {
            actualEntity.setDescription(optDescription.get());
            changeSW[i++] = true;
            log.debug("TemplateDto.description is valid");
        }
        Optional<String> optTemplateTypeId = dto.getTemplateTypeId();
        if(!fieldsToEscape.contains("templateTypeId") && optTemplateTypeId.isPresent()) {
            actualEntity.setTemplateTypeId(optTemplateTypeId.get());
            changeSW[i++] = true;
            log.debug("TemplateDto.templateTypeId is valid");
        }
        Optional<String> optContent = dto.getContent();
        if(!fieldsToEscape.contains("content") && optContent.isPresent()) {
            actualEntity.setContent(optContent.get());
            changeSW[i++] = true;
            log.debug("TemplateDto.content is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("TemplateDto.active is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided TemplateDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided TemplateDto attributes are valid");
    }

}
