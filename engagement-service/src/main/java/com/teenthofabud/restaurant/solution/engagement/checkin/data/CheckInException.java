package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

@Getter
@Setter
@ToString
public class CheckInException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    private Optional<CheckInType> type;

    public CheckInException(String message, CheckInType type) {
        super(message);
        this.type = Optional.of(type);
    }

    public CheckInException(String message, CheckInType type, Object[] parameters) {
        super(message, parameters);
        this.type = Optional.of(type);
    }

    public CheckInException(TOABError error, String message, CheckInType type, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
        this.type = Optional.of(type);
    }

    public CheckInException(TOABError error, CheckInType type, Object[] parameters) {
        super(error, parameters);
        this.error = error;
        this.type = Optional.of(type);
    }

    public CheckInException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
        this.type = Optional.empty();
    }

    @Override
    public String getSubDomain() {
        List<String> elements = new ArrayList<>(Arrays.asList("CheckIn"));
        if(type.isPresent()) {
            elements.add(":");
            elements.add(type.get().name());
        }
        return String.join("-", elements);
    }

}
