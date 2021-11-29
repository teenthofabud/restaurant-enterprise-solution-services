package com.teenthofabud.restaurant.solution.establishmentarea.floor.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString(onlyExplicitlyIncluded = true)
public class FloorException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public FloorException(String message) {
        super(message);
    }

    public FloorException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public FloorException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public FloorException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Floor";
    }

}
