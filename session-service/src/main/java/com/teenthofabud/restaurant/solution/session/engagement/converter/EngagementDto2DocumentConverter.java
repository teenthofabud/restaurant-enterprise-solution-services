package com.teenthofabud.restaurant.solution.session.engagement.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.session.engagement.data.EngagementDocument;
import com.teenthofabud.restaurant.solution.session.engagement.data.EngagementDto;
import com.teenthofabud.restaurant.solution.session.engagement.data.EngagementEvent;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class EngagementDto2DocumentConverter implements ComparativePatchConverter<EngagementDto, EngagementDocument> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 4;

    private List<String> fieldsToEscape;
    private String dateFormat;
    private String timeFormat;

    @Value("#{'${res.settings.discount.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Value("${res.session.association.date.format}")
    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    @Value("${res.session.association.time.format}")
    public void setTimeFormat(String timeFormat) {
        this.timeFormat = timeFormat;
    }


    @Override
    public void compareAndMap(EngagementDto dto, EngagementDocument actualDocument) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;

        Optional<String> optAssociationId = dto.getAssociationId();
        if(!fieldsToEscape.contains("associationId") && optAssociationId.isPresent()) {
            actualDocument.setAssociationId(optAssociationId.get());
            changeSW[i++] = true;
            log.debug("EngagementDto.associationId is valid");
        }

        Optional<String> optEvent = dto.getEvent();
        if(!fieldsToEscape.contains("event") && optEvent.isPresent()) {
            actualDocument.setEvent(EngagementEvent.valueOf(optEvent.get()));
            changeSW[i++] = true;
            log.debug("EngagementDto.event is valid");
        }

        Optional<String> optDate = dto.getDate();
        if(!fieldsToEscape.contains("date") && optDate.isPresent()) {
            LocalDate date = LocalDate.parse(optDate.get(), DateTimeFormatter.ofPattern(dateFormat));
            actualDocument.setDate(date);
            changeSW[i++] = true;
            log.debug("EngagementDto.date is valid");
        }

        Optional<String> optTime = dto.getTime();
        if(!fieldsToEscape.contains("time") && optTime.isPresent()) {
            LocalTime time = LocalTime.parse(optTime.get(), DateTimeFormatter.ofPattern(dateFormat));
            actualDocument.setTime(time);
            changeSW[i++] = true;
            log.debug("EngagementDto.time is valid");
        }

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualDocument.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("EngagementDto.active is valid");
        }

        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided EngagementDto attributes are valid");
            actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided EngagementDto attributes are valid");
    }

}
