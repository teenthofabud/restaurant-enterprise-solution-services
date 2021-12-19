package com.teenthofabud.restaurant.solution.settings.paymentmethod.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class PaymentMethodException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public PaymentMethodException(String message) {
        super(message);
    }

    public PaymentMethodException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public PaymentMethodException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public PaymentMethodException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "PaymentMethod";
    }

}
