package com.teenthofabud.restaurant.solution.booking.experience.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ExperienceException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public ExperienceException(String message) {
        super(message);
    }

    public ExperienceException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public ExperienceException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public ExperienceException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "PaymentMethod";
    }

}
