package com.teenthofabud.restaurant.solution.engagement.engagement.mapper;

import com.teenthofabud.restaurant.solution.engagement.engagement.data.EngagementDocument;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.StringUtils;

import java.util.Optional;
@Slf4j
public abstract class EngagementDocumentSelfMapper<T extends EngagementDocument> {

    protected Optional<? extends EngagementDocument> compareAndMap(T source, T target) {
        boolean changeSW = false;
        String className = target.getClass().getSimpleName();

        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source {}.id is valid", className);
        }
        if(source.getBookingId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getBookingId())) && source.getBookingId().compareTo(target.getBookingId()) != 0) {
            target.setBookingId(source.getBookingId());
            changeSW = true;
            log.debug("Source {}.bookingId is valid", className);
        }
        if(source.getTokenNumber() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getTokenNumber())) && source.getTokenNumber().compareTo(target.getTokenNumber()) != 0) {
            target.setTokenNumber(source.getTokenNumber());
            changeSW = true;
            log.debug("Source {}.tokenNumber is valid", className);
        }

        if(changeSW) {
            log.debug("All provided {} attributes are valid", className);
            return Optional.of(target);
        } else {
            log.debug("Not all provided {} attributes are valid", className);
            return Optional.empty();
        }
    }
}
