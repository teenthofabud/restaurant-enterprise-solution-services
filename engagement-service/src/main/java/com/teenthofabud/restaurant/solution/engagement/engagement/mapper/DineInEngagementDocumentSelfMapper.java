package com.teenthofabud.restaurant.solution.engagement.engagement.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.DineInEngagementDocument;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class DineInEngagementDocumentSelfMapper extends EngagementDocumentSelfMapper implements SingleChannelMapper<DineInEngagementDocument> {

    @Override
    public Optional<DineInEngagementDocument> compareAndMap(DineInEngagementDocument source, DineInEngagementDocument target) {
        boolean changeSW = false;

        Optional<DineInEngagementDocument> optionalDineInEngagementDocument = super.compareAndMap(source, target);
        if(optionalDineInEngagementDocument.isPresent()) {
            target = optionalDineInEngagementDocument.get();
        }

        if(source.getTableId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getTableId())) && source.getTableId().compareTo(target.getTableId()) != 0) {
            target.setTableId(source.getTableId());
            changeSW = true;
            log.debug("Source DineInEngagementDocument.tableId is valid");
        }
        if(source.getNoOfPersons() != null && source.getNoOfPersons().compareTo(target.getNoOfPersons()) != 0) {
            target.setNoOfPersons(source.getNoOfPersons());
            changeSW = true;
            log.debug("Source DineInEngagementDocument.noOfPersons is valid");
        }

        if(changeSW) {
            log.debug("All provided DineInEngagementDocument attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided DineInEngagementDocument attributes are valid");
            return Optional.empty();
        }
    }
}
