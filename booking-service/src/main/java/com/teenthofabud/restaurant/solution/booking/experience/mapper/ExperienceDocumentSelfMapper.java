package com.teenthofabud.restaurant.solution.booking.experience.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceDocument;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class ExperienceDocumentSelfMapper implements SingleChannelMapper<ExperienceDocument> {

    @Override
    public Optional<ExperienceDocument> compareAndMap(ExperienceDocument source, ExperienceDocument target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source ExperienceDocument.id is valid");
        }
        if(source.getName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getName())) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source ExperienceDocument.name is valid");
        }
        if(source.getDescription() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getDescription())) && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source ExperienceDocument.description is valid");
        }
        if(changeSW) {
            log.debug("All provided ExperienceDocument attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided ExperienceDocument attributes are valid");
            return Optional.empty();
        }
    }
}
