package com.teenthofabud.restaurant.solution.settings.device.converter;

import com.teenthofabud.core.common.converter.TOABBaseDocument2VoConverter;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceDocument;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceVo;
import com.teenthofabud.restaurant.solution.settings.utils.SettingsServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class DeviceDocument2VoConverter extends TOABBaseDocument2VoConverter<DeviceDocument, DeviceVo> implements Converter<DeviceDocument, DeviceVo> {

    private List<String> fieldsToEscape;
    private SettingsServiceHelper settingsServiceHelper;

    @Value("#{'${res.settings.device.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }


    @Autowired
    public void setSettingsServiceHelper(SettingsServiceHelper settingsServiceHelper) {
        this.settingsServiceHelper = settingsServiceHelper;
    }

    @Override
    public DeviceVo convert(DeviceDocument entity) {
        DeviceVo vo = new DeviceVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        if(!fieldsToEscape.contains("location")) {
            vo.setLocation(entity.getLocation());
        }
        if(!fieldsToEscape.contains("deviceTypeId")) {
            vo.setDeviceTypeId(entity.getDeviceTypeId());
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

}
