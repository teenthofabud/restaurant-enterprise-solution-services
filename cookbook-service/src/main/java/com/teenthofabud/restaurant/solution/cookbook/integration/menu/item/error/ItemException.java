package com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.error;

import com.teenthofabud.core.common.error.TOABFeignException;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ItemException extends TOABFeignException {

    public ItemException(String errorCode, String errorMessage) {
        super(errorCode, errorMessage);
    }

    public ItemException(String errorCode, String errorMessage, String errorDomain) {
        super(errorCode, errorMessage, errorDomain);
    }
}
