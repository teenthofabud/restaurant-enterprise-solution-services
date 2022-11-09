package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class WalkInDto2EntityConverter extends CheckInDto2EntityConverter<WalkInDto, WalkInEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 3;

    private List<String> fieldsToEscape;

    @Value("#{'${res.engagement.checkIn.walkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected void compareAndMapChild(WalkInDto dto, WalkInEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;

        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent()) {
            actualEntity.setName(optName.get());
            changeSW[i++] = true;
            log.debug("WalkInDto.name is valid");
        }

        Optional<String> optEmailId = dto.getEmailId();
        if(!fieldsToEscape.contains("emailId") && optEmailId.isPresent()) {
            actualEntity.setEmailId(optEmailId.get());
            changeSW[i++] = true;
            log.debug("WalkInDto.emailId is valid");
        }

        Optional<String> optPhoneNumber = dto.getPhoneNumber();
        if(!fieldsToEscape.contains("phoneNumber") && optPhoneNumber.isPresent()) {
            actualEntity.setPhoneNumber(optPhoneNumber.get());
            changeSW[i++] = true;
            log.debug("WalkInDto.phoneNumber is valid");
        }
        log.debug("Not all provided WalkInDto attributes are valid");
    }

    @Override
    public List<String> getFieldsToEscape() {
        return this.fieldsToEscape;
    }

}
