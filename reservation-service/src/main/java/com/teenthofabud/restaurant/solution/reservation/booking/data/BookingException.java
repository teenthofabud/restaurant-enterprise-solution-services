package com.teenthofabud.restaurant.solution.reservation.booking.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class BookingException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public BookingException(String message) {
        super(message);
    }

    public BookingException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public BookingException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public BookingException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Booking";
    }

}
