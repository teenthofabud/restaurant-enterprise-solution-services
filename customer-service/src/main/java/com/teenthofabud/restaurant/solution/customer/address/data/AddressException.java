package com.teenthofabud.restaurant.solution.customer.address.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class AddressException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public AddressException(String message) {
        super(message);
    }

    public AddressException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public AddressException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public AddressException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Address";
    }

}
