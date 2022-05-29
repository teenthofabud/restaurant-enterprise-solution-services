package com.teenthofabud.restaurant.solution.reservation.engagement.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.DineInEngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class DineInEngagementForm2DocumentMapper extends EngagementForm2DocumentMapper<DineInEngagementDocument> implements DualChannelMapper<DineInEngagementDocument, EngagementForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.reservation.engagement.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<DineInEngagementDocument> compareAndMap(DineInEngagementDocument actualDocument, EngagementForm form) {
        DineInEngagementDocument expectedDocument = new DineInEngagementDocument();
        boolean changeSW = false;

        Optional<DineInEngagementDocument> optionalDineInEngagementDocument = (Optional<DineInEngagementDocument>)
                super.compareAndMap(actualDocument, expectedDocument, form);

        if(optionalDineInEngagementDocument.isPresent()) {
            expectedDocument = optionalDineInEngagementDocument.get();
        }

        // comparative copy
        if(!fieldsToEscape.contains("tableId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTableId()))
                && form.getTableId().compareTo(actualDocument.getTableId()) != 0) {
            expectedDocument.setTableId(form.getTableId());
            changeSW = true;
            log.debug("DineInEngagementDocument.tableId: {} is different as DineInEngagementDocument.tableId: {}", form.getTableId(), actualDocument.getTableId());
        } else {
            expectedDocument.setTableId(actualDocument.getTableId());
            log.debug("DineInEngagementDocument.tableId: is unchanged");
        }

        if(!fieldsToEscape.contains("noOfPersons") && form.getNoOfPersons().compareTo(actualDocument.getNoOfPersons()) != 0) {
            expectedDocument.setNoOfPersons(form.getNoOfPersons());
            changeSW = true;
            log.debug("DineInEngagementDocument.noOfPersons: {} is different as DineInEngagementDocument.noOfPersons: {}",
                    form.getNoOfPersons(), actualDocument.getNoOfPersons());
        } else {
            expectedDocument.setNoOfPersons(actualDocument.getNoOfPersons());
            log.debug("DineInEngagementDocument.noOfPersons: is unchanged");
        }

        return changeSW ? Optional.of(expectedDocument) : Optional.empty();
    }

    @Override
    protected List<String> fieldsToEscape() {
        return this.fieldsToEscape;
    }
}
