package com.teenthofabud.restaurant.solution.settings.discount.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class DiscountException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public DiscountException(String message) {
        super(message);
    }

    public DiscountException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public DiscountException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public DiscountException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Discount";
    }

}
