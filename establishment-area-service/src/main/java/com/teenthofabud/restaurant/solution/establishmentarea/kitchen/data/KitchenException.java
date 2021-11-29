package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString(onlyExplicitlyIncluded = true)
public class KitchenException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public KitchenException(String message) {
        super(message);
    }

    public KitchenException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public KitchenException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public KitchenException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Kitchen";
    }
}
