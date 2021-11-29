package com.teenthofabud.restaurant.solution.establishmentarea.floor.converter;

import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class FloorEntity2VoConverter extends TOABBaseEntity2VoConverter<FloorEntity, FloorVo> implements Converter<FloorEntity, FloorVo> {

    private List<String> fieldsToEscape;

    @Value("{'${res.establishment.area.floor.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public FloorVo convert(FloorEntity entity) {
        FloorVo vo = new FloorVo();
        if(!fieldsToEscape.contains("flrId")) {
            vo.setFlrId(entity.getFlrId().toString());
        }
        if(!fieldsToEscape.contains("flrName")) {
            vo.setFlrName(entity.getFlrName());
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
