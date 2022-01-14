package com.teenthofabud.restaurant.solution.cookbook.recipe.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RecipeException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public RecipeException(String message) {
        super(message);
    }

    public RecipeException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public RecipeException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public RecipeException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Recipe";
    }

}
