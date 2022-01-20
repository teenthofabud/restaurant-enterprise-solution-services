package com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.error;

import com.teenthofabud.core.common.error.TOABFeignException;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ProductException extends TOABFeignException {

    public ProductException(String errorCode, String errorMessage) {
        super(errorCode, errorMessage);
    }

    public ProductException(String errorCode, String errorMessage, String errorDomain) {
        super(errorCode, errorMessage, errorDomain);
    }
}
