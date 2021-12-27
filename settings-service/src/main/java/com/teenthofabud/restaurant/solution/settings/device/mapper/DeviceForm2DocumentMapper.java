package com.teenthofabud.restaurant.solution.settings.device.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceDocument;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class DeviceForm2DocumentMapper implements DualChannelMapper<DeviceDocument, DeviceForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.device.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<DeviceDocument> compareAndMap(DeviceDocument actualDocument, DeviceForm form) {
        DeviceDocument expectedDocument = new DeviceDocument();
        boolean changeSW = false;
        // direct copy
        expectedDocument.setId(actualDocument.getId());
        log.debug("Directly copying DeviceDocument.id: {} from actualDocument to expectedDocument", actualDocument.getId());
        expectedDocument.setCreatedOn(actualDocument.getCreatedOn());
        log.debug("Directly copying DeviceDocument.createdOn: {} from actualDocument to expectedDocument", actualDocument.getCreatedOn());
        expectedDocument.setActive(actualDocument.getActive());
        log.debug("Directly copying DeviceDocument.active: {} from actualDocument to expectedDocument", actualDocument.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName()))
                && form.getName().compareTo(actualDocument.getName()) != 0) {
            expectedDocument.setName(form.getName());
            changeSW = true;
            log.debug("DeviceForm.name: {} is different as DeviceDocument.name: {}", form.getName(), actualDocument.getName());
        } else {
            expectedDocument.setName(actualDocument.getName());
            log.debug("DeviceForm.name: is unchanged");
        }
        if(!fieldsToEscape.contains("description") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription()))
                && form.getDescription().compareTo(actualDocument.getDescription()) != 0) {
            expectedDocument.setDescription(form.getDescription());
            changeSW = true;
            log.debug("DeviceForm.description: {} is different as DeviceDocument.description: {}", form.getDescription(), actualDocument.getDescription());
        } else {
            expectedDocument.setDescription(actualDocument.getDescription());
            log.debug("DeviceForm.description: is unchanged");
        }
        if(!fieldsToEscape.contains("deviceTypeId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDeviceTypeId()))
                && form.getDeviceTypeId().compareTo(actualDocument.getDeviceTypeId()) != 0) {
            expectedDocument.setDeviceTypeId(form.getDeviceTypeId());
            changeSW = true;
            log.debug("DeviceForm.deviceTypeId: {} is different as DeviceDocument.deviceTypeId: {}", form.getDeviceTypeId(), actualDocument.getDeviceTypeId());
        } else {
            expectedDocument.setDeviceTypeId(actualDocument.getDeviceTypeId());
            log.debug("DeviceForm.deviceTypeId: is unchanged");
        }
        if(!fieldsToEscape.contains("location") && StringUtils.hasText(StringUtils.trimWhitespace(form.getLocation()))
                && form.getLocation().compareTo(actualDocument.getLocation()) != 0) {
            expectedDocument.setLocation(form.getLocation());
            changeSW = true;
            log.debug("DeviceForm.location: {} is different as DeviceDocument.location: {}", form.getLocation(), actualDocument.getLocation());
        } else {
            expectedDocument.setLocation(actualDocument.getLocation());
            log.debug("DeviceForm.location: is unchanged");
        }
        return changeSW ? Optional.of(expectedDocument) : Optional.empty();
    }

}
