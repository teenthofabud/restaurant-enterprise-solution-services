package com.teenthofabud.restaurant.solution.engagement.tableallocation.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
public class TableAllocationException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    private Optional<CheckInType> type;

    public TableAllocationException(String message, CheckInType type) {
        super(message);
        this.type = Optional.of(type);
    }

    public TableAllocationException(String message, CheckInType type, Object[] parameters) {
        super(message, parameters);
        this.type = Optional.of(type);
    }

    public TableAllocationException(TOABError error, String message, CheckInType type, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
        this.type = Optional.of(type);
    }

    public TableAllocationException(TOABError error, CheckInType type, Object[] parameters) {
        super(error, parameters);
        this.error = error;
        this.type = Optional.of(type);
    }

    public TableAllocationException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
        this.type = Optional.empty();
    }

    @Override
    public String getSubDomain() {
        return "TableAllocation";
    }

}
