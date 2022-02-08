package com.teenthofabud.restaurant.solution.booking.integration.customer.error;

import com.teenthofabud.core.common.error.TOABFeignException;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class AccountException extends TOABFeignException {

    public AccountException(String errorCode, String errorMessage) {
        super(errorCode, errorMessage);
    }

    public AccountException(String errorCode, String errorMessage, String errorDomain) {
        super(errorCode, errorMessage, errorDomain);
    }
}
