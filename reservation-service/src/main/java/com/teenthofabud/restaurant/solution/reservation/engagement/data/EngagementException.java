package com.teenthofabud.restaurant.solution.reservation.engagement.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class EngagementException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public EngagementException(String message) {
        super(message);
    }

    public EngagementException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public EngagementException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public EngagementException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Engagement";
    }

}
