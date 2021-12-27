package com.teenthofabud.restaurant.solution.settings.device.converter;

import com.teenthofabud.restaurant.solution.settings.device.data.DeviceDocument;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class DeviceForm2DocumentConverter implements Converter<DeviceForm, DeviceDocument> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.device.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public DeviceDocument convert(DeviceForm form) {
        DeviceDocument entity = new DeviceDocument();
        if(!fieldsToEscape.contains("name")) {
            entity.setName(form.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            entity.setDescription(form.getDescription());
        }
        if(!fieldsToEscape.contains("deviceTypeId")) {
            entity.setDeviceTypeId(form.getDeviceTypeId());
        }
        if(!fieldsToEscape.contains("location")) {
            entity.setLocation(form.getLocation());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
