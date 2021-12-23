package com.teenthofabud.restaurant.solution.settings.deliverypartner.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class DeliveryPartnerException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public DeliveryPartnerException(String message) {
        super(message);
    }

    public DeliveryPartnerException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public DeliveryPartnerException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public DeliveryPartnerException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "DeliveryPartner";
    }

}
