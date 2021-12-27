package com.teenthofabud.restaurant.solution.settings.device.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class DeviceException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public DeviceException(String message) {
        super(message);
    }

    public DeviceException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public DeviceException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public DeviceException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Device";
    }

}
