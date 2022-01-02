package com.teenthofabud.restaurant.solution.inventory.error;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.handler.TOABBaseWebExceptionHandler;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductException;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class InventoryWebExceptionHandler extends TOABBaseWebExceptionHandler {

    @ExceptionHandler(value = { CategoryException.class, ProductException.class, QuantityException.class })
    public ResponseEntity<ErrorVo> handleCategorySubDomainExceptions(TOABBaseException e) {
        ResponseEntity<ErrorVo>  response = super.parseExceptionToResponse(e);
        return response;
    }

}
