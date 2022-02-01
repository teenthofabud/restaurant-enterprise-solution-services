package com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.error;

import brave.Tracer;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.proxy.TOABFeignBaseExceptionHandler;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.Optional;

@Slf4j
@Component
public class CountryStateCityApiServiceClientExceptionHandler extends TOABFeignBaseExceptionHandler {

    private Tracer tracer;

    @Override
    public HttpStatus[] getClientErrorResponseStatuses() {
        return new HttpStatus[] { HttpStatus.UNAUTHORIZED, HttpStatus.NOT_FOUND };
    }

    @Override
    public HttpStatus[] getServerErrorResponseStatuses() {
        return new HttpStatus[] { HttpStatus.SERVICE_UNAVAILABLE, HttpStatus.INTERNAL_SERVER_ERROR };
    }

    @Autowired
    public void setTracer(Tracer tracer) {
        this.tracer = tracer;
    }


    @Override
    public Optional<ErrorVo> getClientErrorMappedNativeErrorModel(byte[] bytes) {
        Optional<com.teenthofabud.core.common.data.vo.ErrorVo> optErrorDetails = Optional.empty();
        try {
            com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.ErrorVo errorDetails
                    = super.getObjectMapper().readValue(bytes,
                    com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.ErrorVo.class);
            CustomerErrorCode cec = CustomerErrorCode.CUST_ATTRIBUTE_INVALID;
            ErrorVo nativeErrorDetails = new ErrorVo();
            nativeErrorDetails.setCode(cec.getErrorCode());
            nativeErrorDetails.setDomain(cec.getDomain());
            nativeErrorDetails.setMessage(errorDetails.getError());
            nativeErrorDetails.setTrace(tracer.currentSpan().context().traceIdString());
            optErrorDetails = Optional.of(nativeErrorDetails);
        } catch (IOException e) {
            log.error("Error parsing response to native error model", e);
            throw new TOABSystemException(TOABErrorCode.SYSTEM_IO_FAILURE, "Error parsing response to native error model",
                    new Object[] { e.getMessage() });
        }
        return optErrorDetails;
    }
}
