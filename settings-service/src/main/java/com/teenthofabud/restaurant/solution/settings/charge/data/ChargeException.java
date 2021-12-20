package com.teenthofabud.restaurant.solution.settings.charge.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ChargeException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public ChargeException(String message) {
        super(message);
    }

    public ChargeException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public ChargeException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public ChargeException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Charge";
    }

}
