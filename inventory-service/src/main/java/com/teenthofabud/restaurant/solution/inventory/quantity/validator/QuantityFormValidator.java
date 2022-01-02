package com.teenthofabud.restaurant.solution.inventory.quantity.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.inventory.error.InventoryErrorCode;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductException;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductVo;
import com.teenthofabud.restaurant.solution.inventory.product.service.ProductService;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityForm;
import com.teenthofabud.restaurant.solution.inventory.utils.InventoryServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class QuantityFormValidator implements Validator {

    private List<String> fieldsToEscape;
    private InventoryServiceHelper inventoryServiceHelper;
    private ProductService productService;

    @Autowired
    public void setInventoryServiceHelper(InventoryServiceHelper inventoryServiceHelper) {
        this.inventoryServiceHelper = inventoryServiceHelper;
    }

    @Autowired
    public void setProductService(ProductService productService) {
        this.productService = productService;
    }

    @Value("#{'${res.inventory.quantity.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }
    
    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(QuantityForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        QuantityForm form = (QuantityForm) target;
        if(!fieldsToEscape.contains("amount") && (form.getAmount() == null || form.getAmount() <= 0)) {
            log.debug("QuantityForm.amount is empty");
            errors.rejectValue("amount", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("weightId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getWeightId()))) {
            log.debug("QuantityForm.weightId is empty");
            errors.rejectValue("weightId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("weightId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getWeightId()))) {
            if(!inventoryServiceHelper.isWeightCodeValid(form.getWeightId())) {
                log.debug("QuantityForm.weightId is invalid");
                errors.rejectValue("weightId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        if(!fieldsToEscape.contains("productId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getProductId()))) {
            log.debug("QuantityForm.productId is empty");
            errors.rejectValue("productId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("productId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getProductId()))) {
            String productId = form.getProductId();
            try {
                Long.parseLong(productId);
            } catch (NumberFormatException e) {
                log.debug("QuantityForm.productId is invalid");
                errors.rejectValue("productId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return;
            }
            try {
                ProductVo productVo = productService.retrieveDetailsById(productId, Optional.of(TOABCascadeLevel.ONE));
                if(!productVo.getActive()) {
                    log.debug("QuantityForm.productId is inactive");
                    errors.rejectValue("productId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (ProductException e) {
                log.debug("QuantityForm.productId is invalid");
                errors.rejectValue("productId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return;
            }
        }
    }

}
