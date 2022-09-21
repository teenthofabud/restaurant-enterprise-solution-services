package com.teenthofabud.restaurant.solution.engagement.category.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class CategoryException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public CategoryException(String message) {
        super(message);
    }

    public CategoryException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public CategoryException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public CategoryException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "PaymentMethod";
    }

}
