package com.teenthofabud.restaurant.solution.establishmentarea.table.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString(onlyExplicitlyIncluded = true)
public class TableException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public TableException(String message) {
        super(message);
    }

    public TableException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public TableException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public TableException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Table";
    }
}
