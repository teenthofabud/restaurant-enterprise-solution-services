package com.teenthofabud.restaurant.solution.menu.item.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ItemException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public ItemException(String message) {
        super(message);
    }

    public ItemException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public ItemException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public ItemException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Item";
    }

}
