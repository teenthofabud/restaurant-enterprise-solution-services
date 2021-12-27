package com.teenthofabud.restaurant.solution.settings.device.data;

import lombok.Getter;

@Getter
public enum DeviceMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_DEVICE_DOCUMENT_ID("Searching for device document with id: {}"),
    MSG_TEMPLATE_NO_DEVICE_DOCUMENT_ID_AVAILABLE("No device document available with id: {}"),
    MSG_TEMPLATE_FOUND_DEVICE_DOCUMENT_ID("Found device document with id: {}"),
    MSG_TEMPLATE_DEVICE_ID_VALID("device id: {} is semantically valid"),
    MSG_TEMPLATE_DEVICE_ID_INVALID("device id: {} is invalid"),
    MSG_TEMPLATE_DEVICE_ID_EMPTY("device id is empty"),
    MSG_TEMPLATE_DEVICE_CASCADE_LEVEL_EMPTY("device cascadeLevel is empty"),
    MSG_TEMPLATE_DEVICE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID ("Checking existence of device document with name: {} and device type id: {}"),
    MSG_TEMPLATE_DEVICE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID ("device document already exists with name: {} and device type id: {}"),
    MSG_TEMPLATE_DEVICE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID ("No device document exists with name: {} and device type id: {}");

    private String value;

    private DeviceMessageTemplate(String value) {
        this.value = value;
    }


}
