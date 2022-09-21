package com.teenthofabud.restaurant.solution.engagement.engagement.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.EngagementDocument;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.EngagementDto;
import com.teenthofabud.restaurant.solution.engagement.engagement.visitor.EngagementDto2DocumentAssigner;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;

@Component
@Slf4j
public class EngagementDto2DocumentConverter implements ComparativePatchConverter<EngagementDto, EngagementDocument> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 3;

    private List<String> fieldsToEscape;

    @Value("#{'${res.reservation.engagement.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }


    @Override
    public void compareAndMap(EngagementDto dto, EngagementDocument actualDocument) throws TOABBaseException {
        List<Boolean> changeSW = new ArrayList<>(NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS); // size = number of attributes in base dto
        Collections.fill(changeSW, Boolean.FALSE);
        int i = 0;
        EngagementDto2DocumentAssigner engagementDto2DocumentAssigner = new EngagementDto2DocumentAssigner(dto, fieldsToEscape);

        Optional<String> optBookingId = dto.getBookingId();
        if(!fieldsToEscape.contains("bookingId") && optBookingId.isPresent()) {
            actualDocument.setBookingId(optBookingId.get());
            changeSW.add(Boolean.TRUE);
            log.debug("EngagementDto.bookingId is valid");
        }

        Optional<String> optTokenNumber = dto.getTokenNumber();
        if(!fieldsToEscape.contains("tokenNumber") && optTokenNumber.isPresent()) {
            actualDocument.setTokenNumber(optTokenNumber.get());
            changeSW.add(Boolean.TRUE);
            log.debug("EngagementDto.tokenNumber is valid");
        }

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualDocument.setActive(Boolean.valueOf(optActive.get()));
            changeSW.add(Boolean.TRUE);
            log.debug("EngagementDto.active is valid");
        }

        Integer changedFieldCount = actualDocument.assign(engagementDto2DocumentAssigner);

        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) + changedFieldCount >= 1) {
            log.debug("All provided EngagementDto attributes are valid");
            actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided EngagementDto attributes are valid");
    }

}
