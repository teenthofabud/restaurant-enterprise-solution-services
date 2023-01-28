package com.teenthofabud.restaurant.solution.encounter.delivery.mapper;

import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryEntity;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import com.teenthofabud.restaurant.solution.encounter.meeting.mapper.MeetingForm2EntityMapper;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class DeliveryForm2EntityMapper extends MeetingForm2EntityMapper<DeliveryEntity, DeliveryForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.encounter.meeting.delivery.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public List<String> getFieldsToEscape() {
        return this.fieldsToEscape;
    }

    @Override
    public Optional<DeliveryEntity> compareAndMap(DeliveryEntity actualEntity, DeliveryForm deliveryForm) {
        boolean changeSW = false;
        // direct copy of common attributes handled in parent
        DeliveryEntity expectedEntity = new DeliveryEntity();
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying DeliveryEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());

        // parent copy
        Optional<DeliveryEntity> optionalExpectedEntity = super.compareAndMap(actualEntity, expectedEntity, deliveryForm);
        if(optionalExpectedEntity.isPresent()) {
            expectedEntity = optionalExpectedEntity.get();
        }

        if(!fieldsToEscape.contains("orderId") && StringUtils.hasText(StringUtils.trimWhitespace(deliveryForm.getOrderId())) && deliveryForm.getOrderId().compareTo(actualEntity.getOrderId()) != 0) {
            expectedEntity.setOrderId(deliveryForm.getOrderId());
            changeSW = true;
            log.debug("DeliveryForm.orderId: {} is different as DeliveryEntity.orderId: {}", deliveryForm.getOrderId(), actualEntity.getOrderId());
        } else {
            expectedEntity.setOrderId(actualEntity.getOrderId());
            log.debug("DeliveryForm.orderId: is unchanged");
        }

        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
