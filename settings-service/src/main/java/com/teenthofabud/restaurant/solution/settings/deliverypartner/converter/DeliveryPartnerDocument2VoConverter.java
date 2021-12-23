package com.teenthofabud.restaurant.solution.settings.deliverypartner.converter;

import com.teenthofabud.core.common.converter.TOABBaseDocument2VoConverter;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerDocument;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerVo;
import com.teenthofabud.restaurant.solution.settings.utils.SettingsServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class DeliveryPartnerDocument2VoConverter extends TOABBaseDocument2VoConverter<DeliveryPartnerDocument, DeliveryPartnerVo> implements Converter<DeliveryPartnerDocument, DeliveryPartnerVo> {

    private List<String> fieldsToEscape;
    private SettingsServiceHelper settingsServiceHelper;

    @Value("#{'${res.settings.deliverypartner.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }


    @Autowired
    public void setSettingsServiceHelper(SettingsServiceHelper settingsServiceHelper) {
        this.settingsServiceHelper = settingsServiceHelper;
    }

    @Override
    public DeliveryPartnerVo convert(DeliveryPartnerDocument entity) {
        DeliveryPartnerVo vo = new DeliveryPartnerVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

}
