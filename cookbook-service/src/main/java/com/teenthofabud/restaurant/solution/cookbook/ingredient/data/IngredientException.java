package com.teenthofabud.restaurant.solution.cookbook.ingredient.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class IngredientException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public IngredientException(String message) {
        super(message);
    }

    public IngredientException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public IngredientException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public IngredientException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Ingredient";
    }

}
