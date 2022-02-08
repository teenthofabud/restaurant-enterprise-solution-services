package com.teenthofabud.restaurant.solution.customer.integration.metadata.error;

import com.teenthofabud.core.common.error.TOABFeignException;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class GenderException extends TOABFeignException {

    public GenderException(String errorCode, String errorMessage) {
        super(errorCode, errorMessage);
    }

    public GenderException(String errorCode, String errorMessage, String errorDomain) {
        super(errorCode, errorMessage, errorDomain);
    }
}
