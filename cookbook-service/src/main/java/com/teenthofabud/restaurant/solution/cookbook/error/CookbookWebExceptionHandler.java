package com.teenthofabud.restaurant.solution.cookbook.error;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.handler.TOABBaseWebExceptionHandler;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineException;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientException;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class CookbookWebExceptionHandler extends TOABBaseWebExceptionHandler {

    @ExceptionHandler(value = { CuisineException.class, RecipeException.class, IngredientException.class })
    public ResponseEntity<ErrorVo> handleCookbookSubDomainExceptions(TOABBaseException e) {
        ResponseEntity<ErrorVo>  response = super.parseExceptionToResponse(e);
        return response;
    }

}
