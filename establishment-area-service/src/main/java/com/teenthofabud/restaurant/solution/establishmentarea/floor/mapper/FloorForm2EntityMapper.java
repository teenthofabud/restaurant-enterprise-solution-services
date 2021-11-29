package com.teenthofabud.restaurant.solution.establishmentarea.floor.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class FloorForm2EntityMapper implements DualChannelMapper<FloorEntity, FloorForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.establishment.area.floor.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<FloorEntity> compareAndMap(FloorEntity actualEntity, FloorForm form) {
        FloorEntity expectedEntity = new FloorEntity();
        boolean changeSW = false;
        // direct copy
        log.debug("Directly copying FloorEntity.flrId: {} from actualEntity to expectedEntity", actualEntity.getFlrId());
        expectedEntity.setFlrId(actualEntity.getFlrId());
        log.debug("Directly copying FloorEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying FloorEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        expectedEntity.setActive(actualEntity.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("flrName") && StringUtils.hasText(StringUtils.trimWhitespace(form.getFlrName()))
                && form.getFlrName().compareTo(actualEntity.getFlrName()) != 0) {
            expectedEntity.setFlrName(form.getFlrName());
            changeSW = true;
            log.debug("FloorForm.flrName: {} is different as FloorEntity.flrName: {}", form.getFlrName(), actualEntity.getFlrName());
        } else {
            expectedEntity.setFlrName(actualEntity.getFlrName());
            log.debug("FloorForm.flrName: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
