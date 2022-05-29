package com.teenthofabud.restaurant.solution.reservation.error;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.handler.TOABBaseWebExceptionHandler;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingException;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementException;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class ReservationWebExceptionHandler extends TOABBaseWebExceptionHandler {

    @ExceptionHandler(value = { CategoryException.class, EngagementException.class, BookingException.class })
    public ResponseEntity<ErrorVo> handleBookingSubDomainExceptions(TOABBaseException e) {
        ResponseEntity<ErrorVo>  response = super.parseExceptionToResponse(e);
        return response;
    }

}