package com.teenthofabud.restaurant.solution.cookbook.integration.inventory.validator;

import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.data.ProductVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.proxy.InventoryServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class ProductIdValidator implements Validator {

    private InventoryServiceClient inventoryServiceClient;

    @Autowired
    public void setInventoryServiceClient(InventoryServiceClient inventoryServiceClient) {
        this.inventoryServiceClient = inventoryServiceClient;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(String.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        String objectName = errors.getObjectName();
        String productId = (String) target;
        log.debug("Validating productId: {}", productId);
        ProductVo productVo = null;
        log.info("Requesting details of product with id: {}", productId);
        productVo = inventoryServiceClient.getProductDetailsById(productId);
        log.info("Retrieved product: {} by id", productVo);
        if(productVo == null) {
            errors.reject(CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".productId is invalid");
            return;
        }
        boolean emptyFileName = !StringUtils.hasText(StringUtils.trimWhitespace(productVo.getName()));
        boolean emptyId = !StringUtils.hasText(StringUtils.trimWhitespace(productVo.getId()));
        if(emptyFileName) {
            log.debug(objectName + ".product.name is invalid");
            errors.reject(CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return;
        }
        if(emptyId) {
            log.debug(objectName + ".product.productId is invalid");
            errors.reject(CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!productVo.getActive()) {
            errors.reject(CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".productId is not active");
            return;
        }
    }

}
