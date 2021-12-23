package com.teenthofabud.restaurant.solution.settings.deliverypartner.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerDocument;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class DeliveryPartnerForm2DocumentMapper implements DualChannelMapper<DeliveryPartnerDocument, DeliveryPartnerForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.deliverypartner.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<DeliveryPartnerDocument> compareAndMap(DeliveryPartnerDocument actualDocument, DeliveryPartnerForm form) {
        DeliveryPartnerDocument expectedDocument = new DeliveryPartnerDocument();
        boolean changeSW = false;
        // direct copy
        expectedDocument.setId(actualDocument.getId());
        log.debug("Directly copying DeliveryPartnerDocument.id: {} from actualDocument to expectedDocument", actualDocument.getId());
        expectedDocument.setCreatedOn(actualDocument.getCreatedOn());
        log.debug("Directly copying DeliveryPartnerDocument.createdOn: {} from actualDocument to expectedDocument", actualDocument.getCreatedOn());
        expectedDocument.setActive(actualDocument.getActive());
        log.debug("Directly copying DeliveryPartnerDocument.active: {} from actualDocument to expectedDocument", actualDocument.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName()))
                && form.getName().compareTo(actualDocument.getName()) != 0) {
            expectedDocument.setName(form.getName());
            changeSW = true;
            log.debug("DeliveryPartnerForm.name: {} is different as DeliveryPartnerDocument.name: {}", form.getName(), actualDocument.getName());
        } else {
            expectedDocument.setName(actualDocument.getName());
            log.debug("DeliveryPartnerForm.name: is unchanged");
        }
        if(!fieldsToEscape.contains("description") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription()))
                && form.getDescription().compareTo(actualDocument.getDescription()) != 0) {
            expectedDocument.setDescription(form.getDescription());
            changeSW = true;
            log.debug("DeliveryPartnerForm.description: {} is different as DeliveryPartnerDocument.description: {}", form.getDescription(), actualDocument.getDescription());
        } else {
            expectedDocument.setDescription(actualDocument.getDescription());
            log.debug("DeliveryPartnerForm.description: is unchanged");
        }
        return changeSW ? Optional.of(expectedDocument) : Optional.empty();
    }

}
