package com.teenthofabud.restaurant.solution.engagement.handler;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.handler.TOABBaseWebExceptionHandler;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class EngagementWebExceptionHandler extends TOABBaseWebExceptionHandler {

    @ExceptionHandler(value = { CheckInException.class })
    public ResponseEntity<ErrorVo> handleEngagementSubDomainExceptions(TOABBaseException e) {
        ResponseEntity<ErrorVo>  response = super.parseExceptionToResponse(e);
        return response;
    }

}
