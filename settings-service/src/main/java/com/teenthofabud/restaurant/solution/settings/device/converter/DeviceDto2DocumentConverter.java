package com.teenthofabud.restaurant.solution.settings.device.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceDocument;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceDto;
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
public class DeviceDto2DocumentConverter implements ComparativePatchConverter<DeviceDto, DeviceDocument> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 5;

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.device.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public void compareAndMap(DeviceDto dto, DeviceDocument actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent()) {
            actualEntity.setName(optName.get());
            changeSW[i++] = true;
            log.debug("DeviceDto.name is valid");
        }
        Optional<String> optDescription = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDescription.isPresent()) {
            actualEntity.setDescription(optDescription.get());
            changeSW[i++] = true;
            log.debug("DeviceDto.description is valid");
        }
        Optional<String> optDeviceTypeId = dto.getDeviceTypeId();
        if(!fieldsToEscape.contains("deviceTypeId") && optDeviceTypeId.isPresent()) {
            actualEntity.setDeviceTypeId(optDeviceTypeId.get());
            changeSW[i++] = true;
            log.debug("DeviceDto.deviceTypeId is valid");
        }
        Optional<String> optLocation = dto.getLocation();
        if(!fieldsToEscape.contains("location") && optLocation.isPresent()) {
            actualEntity.setLocation(optLocation.get());
            changeSW[i++] = true;
            log.debug("DeviceDto.location is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("DeviceDto.active is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided DeviceDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided DeviceDto attributes are valid");
    }

}
