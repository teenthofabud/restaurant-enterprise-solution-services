package com.teenthofabud.restaurant.solution.establishmentarea.error;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.handler.TOABBaseWebExceptionHandler;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class EstablishmentAreaWebExceptionHandler extends TOABBaseWebExceptionHandler {

    @ExceptionHandler(value = {FloorException.class})
    public ResponseEntity<ErrorVo> handleDownloadSubDomainExceptions(TOABBaseException e) {
        ResponseEntity<ErrorVo>  response = super.parseExceptionToResponse(e);
        return response;
    }

}
