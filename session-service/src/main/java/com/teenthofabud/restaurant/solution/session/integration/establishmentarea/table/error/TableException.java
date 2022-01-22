package com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.error;

import com.teenthofabud.core.common.error.TOABFeignException;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class TableException extends TOABFeignException {

    public TableException(String errorCode, String errorMessage) {
        super(errorCode, errorMessage);
    }

    public TableException(String errorCode, String errorMessage, String errorDomain) {
        super(errorCode, errorMessage, errorDomain);
    }
}
