package com.teenthofabud.restaurant.solution.booking.error;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.handler.TOABBaseWebExceptionHandler;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationException;
import com.teenthofabud.restaurant.solution.booking.engagement.data.EngagementException;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class BookingWebExceptionHandler extends TOABBaseWebExceptionHandler {

    @ExceptionHandler(value = {ExperienceException.class, EngagementException.class, AssociationException.class })
    public ResponseEntity<ErrorVo> handleBookingSubDomainExceptions(TOABBaseException e) {
        ResponseEntity<ErrorVo>  response = super.parseExceptionToResponse(e);
        return response;
    }

}
