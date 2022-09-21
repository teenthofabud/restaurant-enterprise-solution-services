package com.teenthofabud.restaurant.solution.engagement.engagement.mapper;

import com.teenthofabud.restaurant.solution.engagement.engagement.data.EngagementDocument;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.EngagementForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Slf4j
public abstract class EngagementForm2DocumentMapper<T extends EngagementDocument> {

    protected abstract List<String> fieldsToEscape();

    protected Optional<? extends EngagementDocument> compareAndMap(T actualDocument, T expectedDocument, EngagementForm form) {
        String className = actualDocument.getClass().getSimpleName();
        boolean changeSW = false;

        // direct copy
        expectedDocument.setId(actualDocument.getId());
        log.debug("Directly copying {}.id: {} from actualDocument to expectedDocument", className, actualDocument.getId());
        expectedDocument.setCreatedOn(actualDocument.getCreatedOn());
        log.debug("Directly copying {}.createdOn: {} from actualDocument to expectedDocument", className, actualDocument.getCreatedOn());
        expectedDocument.setActive(actualDocument.getActive());
        log.debug("Directly copying {}.active: {} from actualDocument to expectedDocument", className, actualDocument.getActive());
        // comparative copy

        if(!fieldsToEscape().contains("bookingId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getBookingId()))
                && form.getBookingId().compareTo(actualDocument.getBookingId()) != 0) {
            expectedDocument.setBookingId(form.getBookingId());
            changeSW = true;
            log.debug("EngagementForm.bookingId: {} is different as {}.associationId: {}", form.getBookingId(), className, actualDocument.getBookingId());
        } else {
            expectedDocument.setBookingId(actualDocument.getBookingId());
            log.debug("EngagementForm.bookingId: is unchanged");
        }

        if(!fieldsToEscape().contains("tokenNumber") && form.getTokenNumber().compareTo(actualDocument.getTokenNumber()) != 0) {
            expectedDocument.setTokenNumber(form.getTokenNumber());
            changeSW = true;
            log.debug("EngagementForm.tokenNumber: {} is different as {}.event: {}", form.getTokenNumber(), className, actualDocument.getTokenNumber());
        } else {
            expectedDocument.setTokenNumber(actualDocument.getTokenNumber());
            log.debug("EngagementForm.tokenNumber: is unchanged");
        }

        return changeSW ? Optional.of(expectedDocument) : Optional.empty();
    }

}
