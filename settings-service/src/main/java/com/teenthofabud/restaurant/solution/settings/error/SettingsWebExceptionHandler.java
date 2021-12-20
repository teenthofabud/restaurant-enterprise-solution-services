package com.teenthofabud.restaurant.solution.settings.error;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.handler.TOABBaseWebExceptionHandler;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeException;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountException;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class SettingsWebExceptionHandler extends TOABBaseWebExceptionHandler {

    @ExceptionHandler(value = { PaymentMethodException.class, ChargeException.class, DiscountException.class })
    public ResponseEntity<ErrorVo> handleCategorySubDomainExceptions(TOABBaseException e) {
        ResponseEntity<ErrorVo>  response = super.parseExceptionToResponse(e);
        return response;
    }

}
