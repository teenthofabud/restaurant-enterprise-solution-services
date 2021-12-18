package com.teenthofabud.restaurant.solution.menu.error;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.handler.TOABBaseWebExceptionHandler;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemException;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class MenuWebExceptionHandler extends TOABBaseWebExceptionHandler {

    @ExceptionHandler(value = { CategoryException.class, ItemException.class, PriceException.class })
    public ResponseEntity<ErrorVo> handleCategorySubDomainExceptions(TOABBaseException e) {
        ResponseEntity<ErrorVo>  response = super.parseExceptionToResponse(e);
        return response;
    }

}
