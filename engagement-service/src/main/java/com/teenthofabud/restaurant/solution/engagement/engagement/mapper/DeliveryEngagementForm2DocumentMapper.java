package com.teenthofabud.restaurant.solution.engagement.engagement.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.EngagementForm;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.DeliveryEngagementDocument;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class DeliveryEngagementForm2DocumentMapper extends EngagementForm2DocumentMapper<DeliveryEngagementDocument> implements DualChannelMapper<DeliveryEngagementDocument, EngagementForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.reservation.engagement.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<DeliveryEngagementDocument> compareAndMap(DeliveryEngagementDocument actualDocument, EngagementForm form) {
        DeliveryEngagementDocument expectedDocument = new DeliveryEngagementDocument();
        boolean changeSW = false;

        Optional<DeliveryEngagementDocument> optionalDeliveryEngagementDocument = (Optional<DeliveryEngagementDocument>)
                super.compareAndMap(actualDocument, expectedDocument, form);

        if(optionalDeliveryEngagementDocument.isPresent()) {
            expectedDocument = optionalDeliveryEngagementDocument.get();
        }

        // comparative copy
        if(!fieldsToEscape.contains("extRef") && StringUtils.hasText(StringUtils.trimWhitespace(form.getExtRef()))
                && form.getExtRef().compareTo(actualDocument.getExtRef()) != 0) {
            expectedDocument.setExtRef(form.getExtRef());
            changeSW = true;
            log.debug("DeliveryEngagementDocument.extRef: {} is different as DeliveryEngagementDocument.extRef: {}",
                    form.getExtRef(), actualDocument.getExtRef());
        } else {
            expectedDocument.setExtRef(actualDocument.getExtRef());
            log.debug("DeliveryEngagementDocument.extRef: is unchanged");
        }

        return changeSW ? Optional.of(expectedDocument) : Optional.empty();
    }

    @Override
    protected List<String> fieldsToEscape() {
        return this.fieldsToEscape;
    }
}
