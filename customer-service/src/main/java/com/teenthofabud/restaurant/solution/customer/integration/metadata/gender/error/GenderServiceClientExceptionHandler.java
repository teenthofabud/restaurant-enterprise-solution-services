package com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.error;

import com.teenthofabud.core.common.proxy.TOABFeignBaseExceptionHandler;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

@Component
public class GenderServiceClientExceptionHandler extends TOABFeignBaseExceptionHandler {

    @Override
    public HttpStatus[] getClientErrorResponseStatuses() {
        return new HttpStatus[] { HttpStatus.BAD_REQUEST, HttpStatus.NOT_FOUND, HttpStatus.CONFLICT, HttpStatus.UNPROCESSABLE_ENTITY};
    }

    @Override
    public HttpStatus[] getServerErrorResponseStatuses() {
        return new HttpStatus[] { HttpStatus.SERVICE_UNAVAILABLE, HttpStatus.INTERNAL_SERVER_ERROR };
    }
}
