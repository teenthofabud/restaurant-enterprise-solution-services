package com.teenthofabud.restaurant.solution.settings.template.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class TemplateException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public TemplateException(String message) {
        super(message);
    }

    public TemplateException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public TemplateException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public TemplateException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Template";
    }

}
