package com.teenthofabud.restaurant.solution.reservation.engagement.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.TakeAwayEngagementDocument;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class TakeAwayEngagementDocumentSelfMapper extends EngagementDocumentSelfMapper implements SingleChannelMapper<TakeAwayEngagementDocument> {

    @Override
    public Optional<TakeAwayEngagementDocument> compareAndMap(TakeAwayEngagementDocument source, TakeAwayEngagementDocument target) {
        boolean changeSW = false;

        Optional<TakeAwayEngagementDocument> optionalTakeAwayEngagementDocument = super.compareAndMap(source, target);
        if(optionalTakeAwayEngagementDocument.isPresent()) {
            target = optionalTakeAwayEngagementDocument.get();
        }

        if(source.getInstructions() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getInstructions())) && source.getInstructions().compareTo(target.getInstructions()) != 0) {
            target.setInstructions(source.getInstructions());
            changeSW = true;
            log.debug("Source TakeAwayEngagementDocument.instructions is valid");
        }

        if(changeSW) {
            log.debug("All provided TakeAwayEngagementDocument attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided TakeAwayEngagementDocument attributes are valid");
            return Optional.empty();
        }
    }
}
