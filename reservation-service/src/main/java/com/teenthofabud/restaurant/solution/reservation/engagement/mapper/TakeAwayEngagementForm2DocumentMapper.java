package com.teenthofabud.restaurant.solution.reservation.engagement.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.TakeAwayEngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class TakeAwayEngagementForm2DocumentMapper extends EngagementForm2DocumentMapper<TakeAwayEngagementDocument> implements DualChannelMapper<TakeAwayEngagementDocument, EngagementForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.reservation.engagement.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<TakeAwayEngagementDocument> compareAndMap(TakeAwayEngagementDocument actualDocument, EngagementForm form) {
        TakeAwayEngagementDocument expectedDocument = new TakeAwayEngagementDocument();
        boolean changeSW = false;

        Optional<TakeAwayEngagementDocument> optionalTakeAwayEngagementDocument = (Optional<TakeAwayEngagementDocument>)
                super.compareAndMap(actualDocument, expectedDocument, form);

        if(optionalTakeAwayEngagementDocument.isPresent()) {
            expectedDocument = optionalTakeAwayEngagementDocument.get();
        }

        // comparative copy
        if(!fieldsToEscape.contains("instructions") && StringUtils.hasText(StringUtils.trimWhitespace(form.getInstructions()))
                && form.getInstructions().compareTo(actualDocument.getInstructions()) != 0) {
            expectedDocument.setInstructions(form.getInstructions());
            changeSW = true;
            log.debug("TakeAwayEngagementDocument.instructions: {} is different as TakeAwayEngagementDocument.instructions: {}",
                    form.getInstructions(), actualDocument.getInstructions());
        } else {
            expectedDocument.setInstructions(actualDocument.getInstructions());
            log.debug("TakeAwayEngagementDocument.instructions: is unchanged");
        }

        return changeSW ? Optional.of(expectedDocument) : Optional.empty();
    }

    @Override
    protected List<String> fieldsToEscape() {
        return this.fieldsToEscape;
    }
}
