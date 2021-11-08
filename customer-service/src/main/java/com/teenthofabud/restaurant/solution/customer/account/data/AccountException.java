package com.teenthofabud.restaurant.solution.customer.account.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class AccountException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public AccountException(String message) {
        super(message);
    }

    public AccountException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public AccountException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public AccountException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Account";
    }

}
