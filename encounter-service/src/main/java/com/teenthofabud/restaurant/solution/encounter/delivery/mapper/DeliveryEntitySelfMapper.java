package com.teenthofabud.restaurant.solution.encounter.delivery.mapper;

import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryEntity;
import com.teenthofabud.restaurant.solution.encounter.meeting.mapper.MeetingEntitySelfMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

@Component
@Slf4j
public class DeliveryEntitySelfMapper extends MeetingEntitySelfMapper<DeliveryEntity> {

    @Override
    protected void compareAndMapChild(DeliveryEntity source, DeliveryEntity target) {
        boolean changeSW = false;

        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source DeliveryEntity.id is valid");
        }
        if(source.getOrderId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getOrderId()))) {
            target.setOrderId(source.getOrderId());
            changeSW = true;
            log.debug("Source DeliveryEntity.phoneNumber is valid");
        }

        if(changeSW) {
            log.debug("All provided DeliveryEntity attributes are valid");
        } else {
            log.debug("Not all provided DeliveryEntity attributes are valid");
        }
    }
}
