package com.teenthofabud.restaurant.solution.inventory.product.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ProductException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public ProductException(String message) {
        super(message);
    }

    public ProductException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public ProductException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public ProductException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Product";
    }

}
