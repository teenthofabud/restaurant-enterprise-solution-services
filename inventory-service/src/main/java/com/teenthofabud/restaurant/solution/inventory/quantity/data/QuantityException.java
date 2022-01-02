package com.teenthofabud.restaurant.solution.inventory.quantity.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class QuantityException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public QuantityException(String message) {
        super(message);
    }

    public QuantityException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public QuantityException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public QuantityException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Quantity";
    }

}
