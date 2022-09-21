package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import com.teenthofabud.restaurant.solution.engagement.constants.CheckInType;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class CheckInException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    private CheckInType type;

    public CheckInException(String message, CheckInType type) {
        super(message);
        this.type = type;
    }

    public CheckInException(String message, CheckInType type, Object[] parameters) {
        super(message, parameters);
        this.type = type;
    }

    public CheckInException(TOABError error, String message, CheckInType type, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
        this.type = type;
    }

    public CheckInException(TOABError error, CheckInType type, Object[] parameters) {
        super(error, parameters);
        this.error = error;
        this.type = type;
    }

    @Override
    public String getSubDomain() {
        return String.join("-", "Checking", type.name());
    }

}
