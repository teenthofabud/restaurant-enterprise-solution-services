package com.teenthofabud.restaurant.solution.booking.engagement.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.booking.engagement.data.EngagementDocument;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class EngagementDocumentSelfMapper implements SingleChannelMapper<EngagementDocument> {

    @Override
    public Optional<EngagementDocument> compareAndMap(EngagementDocument source, EngagementDocument target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source EngagementDocument.id is valid");
        }
        if(source.getAssociationId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getAssociationId())) && source.getAssociationId().compareTo(target.getAssociationId()) != 0) {
            target.setAssociationId(source.getAssociationId());
            changeSW = true;
            log.debug("Source EngagementDocument.associationId is valid");
        }
        if(source.getEvent() != null && source.getEvent().compareTo(target.getEvent()) != 0) {
            target.setEvent(source.getEvent());
            changeSW = true;
            log.debug("Source EngagementDocument.event is valid");
        }
        if(source.getDate() != null && source.getDate().compareTo(target.getDate()) != 0) {
            target.setDate(source.getDate());
            changeSW = true;
            log.debug("Source EngagementDocument.date is valid");
        }
        if(source.getTime() != null && source.getTime().compareTo(target.getTime()) != 0) {
            target.setTime(source.getTime());
            changeSW = true;
            log.debug("Source EngagementDocument.time is valid");
        }
        if(changeSW) {
            log.debug("All provided EngagementDocument attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided EngagementDocument attributes are valid");
            return Optional.empty();
        }
    }
}
