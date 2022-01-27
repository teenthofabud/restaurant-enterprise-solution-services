package com.teenthofabud.restaurant.solution.session.error;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.handler.TOABBaseWebExceptionHandler;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeException;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerException;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceException;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountException;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodException;
import com.teenthofabud.restaurant.solution.settings.template.data.TemplateException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class SessionWebExceptionHandler extends TOABBaseWebExceptionHandler {

    @ExceptionHandler(value = { PaymentMethodException.class, ChargeException.class, DiscountException.class,
            DeliveryPartnerException.class, TemplateException.class, DeviceException.class })
    public ResponseEntity<ErrorVo> handleCategorySubDomainExceptions(TOABBaseException e) {
        ResponseEntity<ErrorVo>  response = super.parseExceptionToResponse(e);
        return response;
    }

}
