package com.teenthofabud.restaurant.solution.settings.charge.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeDocument;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class ChargeForm2DocumentMapper implements DualChannelMapper<ChargeDocument, ChargeForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.charge.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<ChargeDocument> compareAndMap(ChargeDocument actualDocument, ChargeForm form) {
        ChargeDocument expectedDocument = new ChargeDocument();
        boolean changeSW = false;
        // direct copy
        expectedDocument.setId(actualDocument.getId());
        log.debug("Directly copying ChargeDocument.id: {} from actualDocument to expectedDocument", actualDocument.getId());
        expectedDocument.setCreatedOn(actualDocument.getCreatedOn());
        log.debug("Directly copying ChargeDocument.createdOn: {} from actualDocument to expectedDocument", actualDocument.getCreatedOn());
        expectedDocument.setActive(actualDocument.getActive());
        log.debug("Directly copying ChargeDocument.active: {} from actualDocument to expectedDocument", actualDocument.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName()))
                && form.getName().compareTo(actualDocument.getName()) != 0) {
            expectedDocument.setName(form.getName());
            changeSW = true;
            log.debug("ChargeForm.name: {} is different as ChargeDocument.name: {}", form.getName(), actualDocument.getName());
        } else {
            expectedDocument.setName(actualDocument.getName());
            log.debug("ChargeForm.name: is unchanged");
        }
        if(!fieldsToEscape.contains("description") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription()))
                && form.getDescription().compareTo(actualDocument.getDescription()) != 0) {
            expectedDocument.setDescription(form.getDescription());
            changeSW = true;
            log.debug("ChargeForm.description: {} is different as ChargeDocument.description: {}", form.getDescription(), actualDocument.getDescription());
        } else {
            expectedDocument.setDescription(actualDocument.getDescription());
            log.debug("ChargeForm.description: is unchanged");
        }
        if(!fieldsToEscape.contains("rate") && form.getRate() != null && form.getRate().compareTo(actualDocument.getRate()) != 0) {
            expectedDocument.setRate(form.getRate());
            changeSW = true;
            log.debug("ChargeForm.rate: {} is different as ChargeDocument.rate: {}", form.getRate(), actualDocument.getRate());
        } else {
            expectedDocument.setRate(actualDocument.getRate());
            log.debug("ChargeForm.rate: is unchanged");
        }
        return changeSW ? Optional.of(expectedDocument) : Optional.empty();
    }

}
