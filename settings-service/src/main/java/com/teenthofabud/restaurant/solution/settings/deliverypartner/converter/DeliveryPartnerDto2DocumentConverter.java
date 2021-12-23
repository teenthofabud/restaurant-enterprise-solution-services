package com.teenthofabud.restaurant.solution.settings.deliverypartner.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerDocument;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerDto;
import lombok.extern.slf4j.Slf4j;
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
public class DeliveryPartnerDto2DocumentConverter implements ComparativePatchConverter<DeliveryPartnerDto, DeliveryPartnerDocument> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 3;

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.deliverypartner.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public void compareAndMap(DeliveryPartnerDto dto, DeliveryPartnerDocument actualDocument) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent()) {
            actualDocument.setName(optName.get());
            changeSW[i++] = true;
            log.debug("DeliveryPartnerDto.name is valid");
        }
        Optional<String> optDescription = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDescription.isPresent()) {
            actualDocument.setDescription(optDescription.get());
            changeSW[i++] = true;
            log.debug("DeliveryPartnerDto.description is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualDocument.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("DeliveryPartnerDto.active is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided DeliveryPartnerDto attributes are valid");
            actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided DeliveryPartnerDto attributes are valid");
    }

}
