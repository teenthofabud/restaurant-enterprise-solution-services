package com.teenthofabud.restaurant.solution.encounter.delivery.mapper;

import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryEntity;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import com.teenthofabud.restaurant.solution.encounter.meeting.mapper.MeetingForm2EntityMapper;
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

    @Value("#{'${res.engagement.delivery.walkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<MeetingEntity> compareAndMap(MeetingEntity parent, DeliveryEntity deliveryEntityChild, DeliveryForm deliveryFormChild) {
        DeliveryEntity actualEntity = (DeliveryEntity) deliveryEntityChild;
        DeliveryForm form = deliveryFormChild;
        boolean changeSW = false;
        // direct copy of common attributes handled in parent
        DeliveryEntity expectedEntity = new DeliveryEntity(parent);
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying DeliveryEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());

        if(!fieldsToEscape.contains("orderId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getOrderId())) && form.getOrderId().compareTo(actualEntity.getOrderId()) != 0) {
            expectedEntity.setOrderId(form.getOrderId());
            changeSW = true;
            log.debug("DeliveryForm.orderId: {} is different as DeliveryEntity.orderId: {}", form.getOrderId(), actualEntity.getOrderId());
        } else {
            expectedEntity.setOrderId(actualEntity.getOrderId());
            log.debug("DeliveryForm.orderId: is unchanged");
        }

        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
