package com.teenthofabud.restaurant.solution.session.association.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class AssociationException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public AssociationException(String message) {
        super(message);
    }

    public AssociationException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public AssociationException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public AssociationException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Association";
    }

}
