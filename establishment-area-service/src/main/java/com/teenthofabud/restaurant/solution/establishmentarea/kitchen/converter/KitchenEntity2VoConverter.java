package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.converter;

import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class KitchenEntity2VoConverter extends TOABBaseEntity2VoConverter<KitchenEntity, KitchenVo> implements Converter<KitchenEntity, KitchenVo> {

    private List<String> fieldsToEscape;

    @Value("{'${res.establishment.area.floor.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public KitchenVo convert(KitchenEntity entity) {
        KitchenVo vo = new KitchenVo();
        if(!fieldsToEscape.contains("kitchenId")) {
            vo.setKitchenId(entity.getKitchenId().toString());
        }
        if(!fieldsToEscape.contains("kitchenName")) {
            vo.setKitchenName(entity.getKitchenName());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
