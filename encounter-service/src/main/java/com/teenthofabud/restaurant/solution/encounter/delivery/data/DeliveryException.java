package com.teenthofabud.restaurant.solution.encounter.delivery.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class DeliveryException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public DeliveryException(String message) {
        super(message);
    }

    public DeliveryException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public DeliveryException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public DeliveryException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Delivery";
    }
}
