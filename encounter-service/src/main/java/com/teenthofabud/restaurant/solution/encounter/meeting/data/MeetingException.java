package com.teenthofabud.restaurant.solution.encounter.meeting.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class MeetingException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public MeetingException(String message) {
        super(message);
    }

    public MeetingException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public MeetingException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public MeetingException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Meeting";
    }
}
