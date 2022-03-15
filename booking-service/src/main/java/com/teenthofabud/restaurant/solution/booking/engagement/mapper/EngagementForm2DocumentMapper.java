package com.teenthofabud.restaurant.solution.booking.engagement.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.booking.engagement.data.EngagementDocument;
import com.teenthofabud.restaurant.solution.booking.engagement.data.EngagementEvent;
import com.teenthofabud.restaurant.solution.booking.engagement.data.EngagementForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class EngagementForm2DocumentMapper implements DualChannelMapper<EngagementDocument, EngagementForm> {

    private List<String> fieldsToEscape;
    private String dateFormat;
    private String timeFormat;

    @Value("#{'${res.booking.engagement.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Value("${res.booking.engagement.date.format}")
    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    @Value("${res.booking.engagement.time.format}")
    public void setTimeFormat(String timeFormat) {
        this.timeFormat = timeFormat;
    }

    @Override
    public Optional<EngagementDocument> compareAndMap(EngagementDocument actualDocument, EngagementForm form) {
        EngagementDocument expectedDocument = new EngagementDocument();
        boolean changeSW = false;
        // direct copy
        expectedDocument.setId(actualDocument.getId());
        log.debug("Directly copying EngagementDocument.id: {} from actualDocument to expectedDocument", actualDocument.getId());
        expectedDocument.setCreatedOn(actualDocument.getCreatedOn());
        log.debug("Directly copying EngagementDocument.createdOn: {} from actualDocument to expectedDocument", actualDocument.getCreatedOn());
        expectedDocument.setActive(actualDocument.getActive());
        log.debug("Directly copying EngagementDocument.active: {} from actualDocument to expectedDocument", actualDocument.getActive());
        // comparative copy

        if(!fieldsToEscape.contains("associationId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAssociationId()))
                && form.getAssociationId().compareTo(actualDocument.getAssociationId()) != 0) {
            expectedDocument.setAssociationId(form.getAssociationId());
            changeSW = true;
            log.debug("EngagementForm.associationId: {} is different as EngagementDocument.associationId: {}", form.getAssociationId(), actualDocument.getAssociationId());
        } else {
            expectedDocument.setAssociationId(actualDocument.getAssociationId());
            log.debug("EngagementForm.associationId: is unchanged");
        }

        if(!fieldsToEscape.contains("event") && form.getEvent().toUpperCase().compareTo(actualDocument.getEvent().name()) != 0) {
            expectedDocument.setEvent(EngagementEvent.valueOf(form.getEvent()));
            changeSW = true;
            log.debug("EngagementForm.event: {} is different as EngagementDocument.event: {}", form.getEvent(), actualDocument.getEvent());
        } else {
            expectedDocument.setEvent(actualDocument.getEvent());
            log.debug("EngagementForm.event: is unchanged");
        }

        if(!fieldsToEscape.contains("date") && form.getDate() != null) {
            LocalDate date = LocalDate.parse(form.getDate(), DateTimeFormatter.ofPattern(dateFormat));
            if(actualDocument.getDate().compareTo(date) != 0) {
                expectedDocument.setDate(date);
                changeSW = true;
                log.debug("EngagementForm.date: {} is different as EngagementForm.date: {}", form.getDate(), actualDocument.getDate());
            } else {
                expectedDocument.setDate(actualDocument.getDate());
                log.debug("EngagementForm.date: is unchanged");
            }
        } else {
            expectedDocument.setDate(actualDocument.getDate());
            log.debug("EngagementForm.date: is unchanged");
        }

        if(!fieldsToEscape.contains("time") && form.getTime() != null) {
            LocalTime time = LocalTime.parse(form.getTime(), DateTimeFormatter.ofPattern(timeFormat));
            if(actualDocument.getTime().compareTo(time) != 0) {
                expectedDocument.setTime(time);
                changeSW = true;
                log.debug("EngagementForm.time: {} is different as EngagementForm.time: {}", form.getTime(), actualDocument.getTime());
            } else {
                expectedDocument.setTime(actualDocument.getTime());
                log.debug("EngagementForm.time: is unchanged");
            }
        } else {
            expectedDocument.setTime(actualDocument.getTime());
            log.debug("EngagementForm.time: is unchanged");
        }

        return changeSW ? Optional.of(expectedDocument) : Optional.empty();
    }

}
