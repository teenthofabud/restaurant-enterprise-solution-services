package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.repository.FloorRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenDto;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class KitchenDto2EntityConverter implements ComparativePatchConverter<KitchenDto, KitchenEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 4;

    private List<String> fieldsToEscape;

    @Value("#{'${res.establishment.area.kitchen.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private FloorRepository floorRepository;

    @Autowired
    public void setFloorRepository(FloorRepository floorRepository) {
        this.floorRepository = floorRepository;
    }

    @Override
    public void compareAndMap(KitchenDto dto, KitchenEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optKitchenName = dto.getKitchenName();
        if(!fieldsToEscape.contains("kitchenName") && optKitchenName.isPresent()) {
            actualEntity.setKitchenName(optKitchenName.get());
            changeSW[i++] = true;
            log.debug("KitchenDto.kitchenName is valid");
        }

        Optional<String> optDesc = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDesc.isPresent()) {
            actualEntity.setDescription(optDesc.get());
            changeSW[i++] = true;
            log.debug("KitchenDto.description is valid");
        }

        Optional<String> optFloorId = dto.getFloorId();
        if(!fieldsToEscape.contains("floorId") && optFloorId.isPresent()) {
            Long accountId = Long.parseLong(optFloorId.get());
            Optional<FloorEntity> optAccountEntity = floorRepository.findById(accountId);
            actualEntity.setFloor(optAccountEntity.get());
            changeSW[i++] = true;
            log.debug("KitchenDto.floorId is valid");
        }

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("KitchenDto.active is valid");
        }

        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided KitchenDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided KitchenDto attributes are valid");
    }

}
