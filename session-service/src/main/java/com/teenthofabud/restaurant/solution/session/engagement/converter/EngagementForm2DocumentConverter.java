package com.teenthofabud.restaurant.solution.session.engagement.converter;

import com.teenthofabud.restaurant.solution.session.engagement.data.EngagementDocument;
import com.teenthofabud.restaurant.solution.session.engagement.data.EngagementEvent;
import com.teenthofabud.restaurant.solution.session.engagement.data.EngagementForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

@Component
@Slf4j
public class EngagementForm2DocumentConverter implements Converter<EngagementForm, EngagementDocument> {

    private List<String> fieldsToEscape;
    private String dateFormat;
    private String timeFormat;

    @Value("#{'${res.session.association.fields-to-escape}'.split(',')}")
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
    public EngagementDocument convert(EngagementForm form) {
        EngagementDocument entity = new EngagementDocument();
        if(!fieldsToEscape.contains("associationId")) {
            entity.setAssociationId(form.getAssociationId());
        }
        if(!fieldsToEscape.contains("event")) {
            entity.setEvent(EngagementEvent.valueOf(form.getEvent()));
        }
        if(!fieldsToEscape.contains("date")) {
            LocalDate date = LocalDate.parse(form.getDate(), DateTimeFormatter.ofPattern(dateFormat));
            entity.setDate(date);
        }
        if(!fieldsToEscape.contains("time")) {
            LocalTime time = LocalTime.parse(form.getTime(), DateTimeFormatter.ofPattern(timeFormat));
            entity.setTime(time);
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
