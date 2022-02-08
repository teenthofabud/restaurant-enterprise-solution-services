package com.teenthofabud.restaurant.solution.booking.association.converter;

import com.teenthofabud.restaurant.solution.booking.association.data.AssociationDocument;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class AssociationForm2DocumentConverter implements Converter<AssociationForm, AssociationDocument> {

    private List<String> fieldsToEscape;
    //private String endedOnFormat;

    @Value("#{'${res.session.association.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    /*@Value("${res.booking.association.endedon.format}")
    public void setEndedOnFormat(String endedOnFormat) {
        this.endedOnFormat = endedOnFormat;
    }*/

    @Override
    public AssociationDocument convert(AssociationForm form) {
        AssociationDocument entity = new AssociationDocument();
        if(!fieldsToEscape.contains("experienceId")) {
            entity.setExperienceId(form.getExperienceId());
        }
        if(!fieldsToEscape.contains("tableId")) {
            entity.setTableId(form.getTableId());
        }
        if(!fieldsToEscape.contains("accountId")) {
            entity.setAccountId(form.getAccountId());
        }
        /*if(!fieldsToEscape.contains("endedOn")) {
            LocalDateTime endedOn = LocalDateTime.parse(form.getEndedOn(), DateTimeFormatter.ofPattern(endedOnFormat));
            entity.setEndedOn(endedOn);
        }*/
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
