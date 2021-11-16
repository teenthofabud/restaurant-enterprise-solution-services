package com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.error;

import com.teenthofabud.core.common.error.TOABFeignException;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class CountryStateCityApiException extends TOABFeignException {

    public CountryStateCityApiException(String errorCode, String errorMessage) {
        super(errorCode, errorMessage);
    }

    public CountryStateCityApiException(String errorCode, String errorMessage, String errorDomain) {
        super(errorCode, errorMessage, errorDomain);
    }
}
