package com.teenthofabud.restaurant.solution.reservation.engagement.mapper;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingException;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.reservation.booking.service.BookingService;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.DeliveryEngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.DineInEngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.TakeAwayEngagementDocument;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
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
