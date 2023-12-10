package com.teenthofabud.restaurant.solution.settings.internationalization.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.*;

@Getter
@Setter
@ToString
public class LanguageException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public LanguageException(String message) {
        super(message);
    }

    public LanguageException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public LanguageException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
    }

    public LanguageException(TOABError error, Object[] parameter) {
        super(error, parameter);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Language";
    }
}
