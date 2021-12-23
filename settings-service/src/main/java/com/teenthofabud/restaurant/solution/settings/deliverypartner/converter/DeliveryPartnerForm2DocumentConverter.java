package com.teenthofabud.restaurant.solution.settings.deliverypartner.converter;

import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerDocument;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class DeliveryPartnerForm2DocumentConverter implements Converter<DeliveryPartnerForm, DeliveryPartnerDocument> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.deliverypartner.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public DeliveryPartnerDocument convert(DeliveryPartnerForm form) {
        DeliveryPartnerDocument entity = new DeliveryPartnerDocument();
        if(!fieldsToEscape.contains("name")) {
            entity.setName(form.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            entity.setDescription(form.getDescription());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
