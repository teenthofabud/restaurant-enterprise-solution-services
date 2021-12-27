package com.teenthofabud.restaurant.solution.settings.device.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceDocument;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class DeviceDocumentSelfMapper implements SingleChannelMapper<DeviceDocument> {

    @Override
    public Optional<DeviceDocument> compareAndMap(DeviceDocument source, DeviceDocument target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source DeviceDocument.id is valid");
        }
        if(source.getName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getName())) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source DeviceDocument.name is valid");
        }
        if(source.getDescription() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getDescription())) && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source DeviceDocument.description is valid");
        }
        if(source.getDeviceTypeId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getDeviceTypeId())) && source.getDeviceTypeId().compareTo(target.getDeviceTypeId()) != 0) {
            target.setDeviceTypeId(source.getDeviceTypeId());
            changeSW = true;
            log.debug("Source DeviceDocument.deviceTypeId is valid");
        }
        if(source.getLocation() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getLocation())) && source.getLocation().compareTo(target.getLocation()) != 0) {
            target.setLocation(source.getLocation());
            changeSW = true;
            log.debug("Source DeviceDocument.location is valid");
        }
        if(changeSW) {
            log.debug("All provided DeviceDocument attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided DeviceDocument attributes are valid");
            return Optional.empty();
        }
    }
}
