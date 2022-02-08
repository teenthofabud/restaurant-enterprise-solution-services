package com.teenthofabud.restaurant.solution.booking.association.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationDocument;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class AssociationForm2DocumentMapper implements DualChannelMapper<AssociationDocument, AssociationForm> {

    private List<String> fieldsToEscape;
    //private String endedOnFormat;

    @Value("#{'${res.settings.discount.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    /*@Value("${res.booking.association.endednon.format}")
    public void setEndedOnFormat(String endedOnFormat) {
        this.endedOnFormat = endedOnFormat;
    }*/

    @Override
    public Optional<AssociationDocument> compareAndMap(AssociationDocument actualDocument, AssociationForm form) {
        AssociationDocument expectedDocument = new AssociationDocument();
        boolean changeSW = false;
        // direct copy
        expectedDocument.setId(actualDocument.getId());
        log.debug("Directly copying AssociationDocument.id: {} from actualDocument to expectedDocument", actualDocument.getId());
        expectedDocument.setCreatedOn(actualDocument.getCreatedOn());
        log.debug("Directly copying AssociationDocument.createdOn: {} from actualDocument to expectedDocument", actualDocument.getCreatedOn());
        expectedDocument.setActive(actualDocument.getActive());
        log.debug("Directly copying AssociationDocument.active: {} from actualDocument to expectedDocument", actualDocument.getActive());
        // comparative copy

        if(!fieldsToEscape.contains("experienceId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getExperienceId()))
                && form.getExperienceId().compareTo(actualDocument.getExperienceId()) != 0) {
            expectedDocument.setExperienceId(form.getExperienceId());
            changeSW = true;
            log.debug("AssociationForm.experienceId: {} is different as AssociationDocument.experienceId: {}", form.getExperienceId(), actualDocument.getExperienceId());
        } else {
            expectedDocument.setExperienceId(actualDocument.getExperienceId());
            log.debug("AssociationForm.experienceId: is unchanged");
        }

        if(!fieldsToEscape.contains("tableId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTableId()))
                && form.getTableId().compareTo(actualDocument.getTableId()) != 0) {
            expectedDocument.setTableId(form.getTableId());
            changeSW = true;
            log.debug("AssociationForm.tableId: {} is different as AssociationDocument.tableId: {}", form.getTableId(), actualDocument.getTableId());
        } else {
            expectedDocument.setTableId(actualDocument.getTableId());
            log.debug("AssociationForm.tableId: is unchanged");
        }

        if(!fieldsToEscape.contains("accountId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))
                && form.getAccountId().compareTo(actualDocument.getAccountId()) != 0) {
            expectedDocument.setAccountId(form.getAccountId());
            changeSW = true;
            log.debug("AssociationForm.accountId: {} is different as AssociationDocument.accountId: {}", form.getAccountId(), actualDocument.getAccountId());
        } else {
            expectedDocument.setAccountId(actualDocument.getAccountId());
            log.debug("AssociationForm.accountId: is unchanged");
        }

        /*if(!fieldsToEscape.contains("endedOn") && form.getEndedOn() != null) {
            LocalDateTime localDate = LocalDateTime.parse(form.getEndedOn(), DateTimeFormatter.ofPattern(endedOnFormat));
            if(actualDocument.getEndedOn().compareTo(localDate) != 0) {
                expectedDocument.setEndedOn(localDate);
                changeSW = true;
                log.debug("AssociationForm.endedOn: {} is different as AssociationForm.endedOn: {}", form.getEndedOn(), actualDocument.getEndedOn());
            } else {
                expectedDocument.setEndedOn(actualDocument.getEndedOn());
                log.debug("AssociationForm.endedOn: is unchanged");
            }
        } else {
            expectedDocument.setEndedOn(actualDocument.getEndedOn());
            log.debug("AssociationForm.endedOn: is unchanged");
        }*/

        return changeSW ? Optional.of(expectedDocument) : Optional.empty();
    }

}
