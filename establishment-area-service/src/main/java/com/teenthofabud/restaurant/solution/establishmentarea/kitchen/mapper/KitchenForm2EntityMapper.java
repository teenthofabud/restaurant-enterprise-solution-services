package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.repository.FloorRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class KitchenForm2EntityMapper implements DualChannelMapper<KitchenEntity, KitchenForm> {

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
    public Optional<KitchenEntity> compareAndMap(KitchenEntity actualEntity, KitchenForm form) {
        KitchenEntity expectedEntity = new KitchenEntity();
        boolean changeSW = false;
        // direct copy
        log.debug("Directly copying KitchenEntity.kitchenId: {} from actualEntity to expectedEntity", actualEntity.getKitchenId());
        expectedEntity.setKitchenId(actualEntity.getKitchenId());
        log.debug("Directly copying KitchenEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying KitchenEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        expectedEntity.setActive(actualEntity.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("kitchenName") && StringUtils.hasText(StringUtils.trimWhitespace(form.getKitchenName()))
                && form.getKitchenName().compareTo(actualEntity.getKitchenName()) != 0) {
            expectedEntity.setKitchenName(form.getKitchenName());
            changeSW = true;
            log.debug("KitchenForm.kitchenName: {} is different as KitchenEntity.kitchenName: {}", form.getKitchenName(), actualEntity.getKitchenName());
        } else {
            expectedEntity.setKitchenName(actualEntity.getKitchenName());
            log.debug("KitchenForm.kitchenName: is unchanged");
        }
        
        if(!fieldsToEscape.contains("description") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription()))
                && form.getDescription().compareTo(actualEntity.getDescription()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("KitchenForm.description: {} is different as KitchenEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            log.debug("KitchenForm.description: is unchanged");
        }

        if(!fieldsToEscape.contains("floorId") && form.getFloorId() != null) {
            Long floorId = Long.parseLong(form.getFloorId());
            Optional<FloorEntity> optFloorEntity = floorRepository.findById(floorId);
            if(actualEntity.getFloor().compareTo(optFloorEntity.get()) != 0) {
                expectedEntity.setFloor(optFloorEntity.get());
                changeSW = true;
                log.debug("KitchenForm.floorId: {} is different as FloorEntity.floorId: {}", form.getFloorId(), actualEntity.getFloor().getFlrId());
            } else {
                expectedEntity.setFloor(actualEntity.getFloor());
                log.debug("FloorForm.floorId: is unchanged");
            }
        } else {
            expectedEntity.setFloor(actualEntity.getFloor());
            log.debug("FloorForm.floorId: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
