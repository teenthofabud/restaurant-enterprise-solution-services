package com.teenthofabud.restaurant.solution.reservation.engagement.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.DeliveryEngagementDocument;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class DeliveryEngagementDocumentSelfMapper extends EngagementDocumentSelfMapper implements SingleChannelMapper<DeliveryEngagementDocument> {

    @Override
    public Optional<DeliveryEngagementDocument> compareAndMap(DeliveryEngagementDocument source, DeliveryEngagementDocument target) {
        boolean changeSW = false;

        Optional<DeliveryEngagementDocument> optionalDeliveryEngagementDocument = super.compareAndMap(source, target);
        if(optionalDeliveryEngagementDocument.isPresent()) {
            target = optionalDeliveryEngagementDocument.get();
        }

        if(source.getExtRef() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getExtRef())) && source.getExtRef().compareTo(target.getExtRef()) != 0) {
            target.setExtRef(source.getExtRef());
            changeSW = true;
            log.debug("Source DeliveryEngagementDocument.extRef is valid");
        }

        if(changeSW) {
            log.debug("All provided DeliveryEngagementDocument attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided DeliveryEngagementDocument attributes are valid");
            return Optional.empty();
        }
    }
}
