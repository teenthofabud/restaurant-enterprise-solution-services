package com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class CuisineException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public CuisineException(String message) {
        super(message);
    }

    public CuisineException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public CuisineException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public CuisineException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Cuisine";
    }

}
