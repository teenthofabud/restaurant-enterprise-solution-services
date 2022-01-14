package com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.error;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.proxy.TOABFeignBaseExceptionHandler;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.Optional;

@Slf4j
@Component
public class ItemServiceClientExceptionHandler extends TOABFeignBaseExceptionHandler {

    @Override
    public HttpStatus[] getClientErrorResponseStatuses() {
        return new HttpStatus[] { HttpStatus.BAD_REQUEST, HttpStatus.NOT_FOUND, HttpStatus.CONFLICT, HttpStatus.UNPROCESSABLE_ENTITY};
    }

    @Override
    public HttpStatus[] getServerErrorResponseStatuses() {
        return new HttpStatus[] { HttpStatus.SERVICE_UNAVAILABLE, HttpStatus.INTERNAL_SERVER_ERROR };
    }

    @Override
    public Optional<ErrorVo> getClientErrorMappedNativeErrorModel(byte[] bytes) {
        Optional<ErrorVo> optErrorDetails = Optional.empty();
        try {
            ErrorVo errorDetails = super.getObjectMapper().readValue(bytes, ErrorVo.class);
            optErrorDetails = Optional.of(errorDetails);
        } catch (IOException e) {
            log.error("Error parsing response to native error model", e);
            throw new TOABSystemException(TOABErrorCode.SYSTEM_IO_FAILURE, "Error parsing response to native error model",
                    new Object[] { e.getMessage() });
        }
        return optErrorDetails;
    }

}
