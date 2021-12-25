package com.teenthofabud.restaurant.solution.print.template.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class TemplateEntitySelfMapper implements SingleChannelMapper<TemplateEntity> {

    @Override
    public Optional<TemplateEntity> compareAndMap(TemplateEntity source, TemplateEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source TemplateEntity.id is valid");
        }
        if(source.getName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getName())) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source TemplateEntity.name is valid");
        }
        if(source.getDescription() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getDescription())) && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source TemplateEntity.description is valid");
        }
        if(source.getTemplateTypeId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getTemplateTypeId())) && source.getTemplateTypeId().compareTo(target.getTemplateTypeId()) != 0) {
            target.setTemplateTypeId(source.getTemplateTypeId());
            changeSW = true;
            log.debug("Source TemplateEntity.templateTypeId is valid");
        }
        if(source.getContent() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getContent())) && source.getContent().compareTo(target.getContent()) != 0) {
            target.setContent(source.getContent());
            changeSW = true;
            log.debug("Source TemplateEntity.content is valid");
        }
        if(changeSW) {
            log.debug("All provided TemplateEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided TemplateEntity attributes are valid");
            return Optional.empty();
        }
    }
}
