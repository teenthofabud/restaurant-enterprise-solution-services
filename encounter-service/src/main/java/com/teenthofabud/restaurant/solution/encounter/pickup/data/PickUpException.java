package com.teenthofabud.restaurant.solution.encounter.pickup.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class PickUpException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public PickUpException(String message) {
        super(message);
    }

    public PickUpException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public PickUpException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public PickUpException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "PickUp";
    }
}
