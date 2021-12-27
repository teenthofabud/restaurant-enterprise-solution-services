package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.converter;

import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.repository.FloorRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class KitchenForm2EntityConverter implements Converter<KitchenForm, KitchenEntity> {

    private List<String> fieldsToEscape;
    private FloorRepository floorRepository;

    @Value("#{'${res.establishment.area.kitchen.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setFloorRepository(FloorRepository floorRepository) {
        this.floorRepository = floorRepository;
    }

    @Override
    public KitchenEntity convert(KitchenForm form) {
        KitchenEntity entity = new KitchenEntity();
        if(!fieldsToEscape.contains("kitchenName")) {
            entity.setKitchenName(form.getKitchenName());
        }
        if(!fieldsToEscape.contains("description")) {
            entity.setDescription(form.getDescription());
        }
        if(!fieldsToEscape.contains("floorId")) {
            Long floorId = Long.parseLong(form.getFloorId());
            Optional<FloorEntity> optFloorEntity = floorRepository.findById(floorId);
            entity.setFloor(optFloorEntity.get());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
