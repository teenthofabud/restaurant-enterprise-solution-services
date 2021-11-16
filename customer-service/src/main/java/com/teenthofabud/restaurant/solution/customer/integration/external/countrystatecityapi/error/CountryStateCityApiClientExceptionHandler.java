package com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.error;

import com.teenthofabud.core.common.proxy.TOABFeignBaseExceptionHandler;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

@Component
public class CountryStateCityApiClientExceptionHandler extends TOABFeignBaseExceptionHandler {

    @Override
    public HttpStatus[] getClientErrorResponseStatuses() {
        return new HttpStatus[] { HttpStatus.UNAUTHORIZED, HttpStatus.NOT_FOUND };
    }

    @Override
    public HttpStatus[] getServerErrorResponseStatuses() {
        return new HttpStatus[] { HttpStatus.SERVICE_UNAVAILABLE, HttpStatus.INTERNAL_SERVER_ERROR };
    }
}
