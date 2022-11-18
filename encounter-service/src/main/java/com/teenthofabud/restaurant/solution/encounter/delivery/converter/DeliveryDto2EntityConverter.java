package com.teenthofabud.restaurant.solution.encounter.delivery.converter;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryDto;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryEntity;
import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingDto2EntityConverter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class DeliveryDto2EntityConverter extends MeetingDto2EntityConverter<DeliveryDto, DeliveryEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 10;

    private List<String> fieldsToEscape;

    @Value("#{'${res.engagement.checkIn.walkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected void compareAndMapChild(DeliveryDto dto, DeliveryEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;

        Optional<String> optName = dto.getOrderId();
        if(!fieldsToEscape.contains("orderId") && optName.isPresent()) {
            actualEntity.setOrderId(optName.get());
            changeSW[i++] = true;
            log.debug("DeliveryDto.orderId is valid");
        }
        log.debug("Not all provided DeliveryDto attributes are valid");
    }

}
