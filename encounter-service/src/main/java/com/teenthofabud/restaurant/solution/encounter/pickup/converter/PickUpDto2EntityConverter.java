package com.teenthofabud.restaurant.solution.encounter.pickup.converter;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingDto2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpDto;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class PickUpDto2EntityConverter extends MeetingDto2EntityConverter<PickUpDto, PickUpEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 10;

    private List<String> fieldsToEscape;

    @Value("#{'${res.encounter.meeting.pickUp.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected void compareAndMapChild(PickUpDto dto, PickUpEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;

        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent()) {
            actualEntity.setName(optName.get());
            changeSW[i++] = true;
            log.debug("PickUpDto.name is valid");
        }

        Optional<String> optPhoneNumber = dto.getPhoneNo();
        if(!fieldsToEscape.contains("phoneNo") && optPhoneNumber.isPresent()) {
            actualEntity.setPhoneNo(optPhoneNumber.get());
            changeSW[i++] = true;
            log.debug("PickUpDto.phoneNo is valid");
        }
        log.debug("Not all provided PickUpDto attributes are valid");
    }

}
