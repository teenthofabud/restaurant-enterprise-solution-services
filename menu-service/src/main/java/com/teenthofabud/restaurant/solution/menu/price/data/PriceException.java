package com.teenthofabud.restaurant.solution.menu.price.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class PriceException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public PriceException(String message) {
        super(message);
    }

    public PriceException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public PriceException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public PriceException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Price";
    }

}
