package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class KitchenEntitySelfMapper implements SingleChannelMapper<KitchenEntity> {

    @Override
    public Optional<KitchenEntity> compareAndMap(KitchenEntity source, KitchenEntity target) {
        boolean changeSW = false;
        if(source.getKitchenId() != null && source.getKitchenId().compareTo(target.getKitchenId()) != 0) {
            target.setKitchenId(source.getKitchenId());
            changeSW = true;
            log.debug("Source KitchenEntity.kitchenId is valid");
        }if(source.getKitchenName() != null &&
                StringUtils.hasText(StringUtils.trimWhitespace(source.getKitchenName()))
                && source.getKitchenName().compareTo(target.getKitchenName()) != 0) {
            target.setKitchenName(source.getKitchenName());
            changeSW = true;
            log.debug("Source KitchenEntity.kitchenName is valid");
        }
        if(source.getFloor() != null && source.getFloor().compareTo(target.getFloor()) != 0) {
            target.setFloor(source.getFloor());
            changeSW = true;
            log.debug("Source TableEntity.floor is valid");
        }
        if(changeSW) {
            log.debug("All provided KitchenEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided KitchenEntity attributes are valid");
            return Optional.empty();
        }
    }
}
