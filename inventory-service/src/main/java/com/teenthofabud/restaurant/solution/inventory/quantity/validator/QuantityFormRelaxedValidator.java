package com.teenthofabud.restaurant.solution.inventory.quantity.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.validator.RelaxedValidator;
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

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class QuantityFormRelaxedValidator implements RelaxedValidator<QuantityForm>  {

    private List<String> fieldsToEscape;
    private ProductService productService;
    private InventoryServiceHelper inventoryServiceHelper;

    @Value("#{'${res.inventory.quantity.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setInventoryServiceHelper(InventoryServiceHelper inventoryServiceHelper) {
        this.inventoryServiceHelper = inventoryServiceHelper;
    }
    
    @Autowired
    public void setProductService(ProductService productService) {
        this.productService = productService;
    }

    @Override
    public Boolean validateLoosely(QuantityForm form, Errors errors) {
        if(!fieldsToEscape.contains("amount") && form.getAmount() != null && form.getAmount() <= 0) {
            errors.rejectValue("amount", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            log.debug("QuantityForm.amount is empty");
            return false;
        }
        log.debug("QuantityForm.amount is valid");

        if(!fieldsToEscape.contains("weightId") && form.getWeightId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getWeightId()))) {
            errors.rejectValue("weightId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            log.debug("QuantityForm.weightId is empty");
            return false;
        } else if(!fieldsToEscape.contains("weightId") && form.getWeightId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getWeightId()))) {
            if(!inventoryServiceHelper.isWeightCodeValid(form.getWeightId())) {
                log.debug("QuantityForm.weightId is invalid");
                errors.rejectValue("weightId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("QuantityForm.weightId is valid");

        if(!fieldsToEscape.contains("productId") && form.getProductId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getProductId()))) {
            log.debug("QuantityForm.productId is empty");
            errors.rejectValue("productId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("productId") && form.getProductId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getProductId()))) {
            String productId = form.getProductId();
            try {
                Long.parseLong(productId);
            } catch (NumberFormatException e) {
                log.debug("QuantityForm.productId is invalid");
                errors.rejectValue("productId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return false;
            }
            try {
                ProductVo productVo = productService.retrieveDetailsById(productId, Optional.of(TOABCascadeLevel.ONE));
                if(!productVo.getActive()) {
                    log.debug("QuantityForm.productId is inactive");
                    errors.rejectValue("productId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                    return false;
                }
            } catch (ProductException e) {
                log.debug("QuantityForm.productId is invalid");
                errors.rejectValue("productId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        return true;
    }
}
