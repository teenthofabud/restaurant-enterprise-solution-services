package com.teenthofabud.restaurant.solution.encounter.handler;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.handler.TOABBaseWebExceptionHandler;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class EnconterWebExceptionHandler extends TOABBaseWebExceptionHandler {

    @ExceptionHandler(value = { MeetingException.class })
    public ResponseEntity<ErrorVo> handleEncounterSubDomainExceptions(TOABBaseException e) {
        ResponseEntity<ErrorVo>  response = super.parseExceptionToResponse(e);
        return response;
    }

}
