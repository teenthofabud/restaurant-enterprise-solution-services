package com.teenthofabud.restaurant.solution.inventory.quantity.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.inventory.error.InventoryErrorCode;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductException;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductVo;
import com.teenthofabud.restaurant.solution.inventory.product.service.ProductService;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityDto;
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
public class QuantityDtoValidator implements Validator {

    private List<String> fieldsToEscape;
    private ProductService productService;
    private InventoryServiceHelper inventoryServiceHelper;

    @Value("#{'${res.inventory.quantity.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setProductService(ProductService productService) {
        this.productService = productService;
    }

    @Autowired
    public void setInventoryServiceHelper(InventoryServiceHelper inventoryServiceHelper) {
        this.inventoryServiceHelper = inventoryServiceHelper;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(QuantityDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        QuantityDto dto = (QuantityDto) target;
        Optional<String> optAmount = dto.getAmount();
        if(!fieldsToEscape.contains("amount") && optAmount.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optAmount.get()))) {
            errors.rejectValue("amount", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            log.debug("QuantityDto.amount is invalid");
            return;
        } if(!fieldsToEscape.contains("amount") && optAmount.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optAmount.get()))) {
            try {
                Double quantity = Double.parseDouble(optAmount.get());
                if(quantity <= 0.0d) {
                    errors.rejectValue("amount", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                    log.debug("QuantityDto.amount is invalid");
                    return;
                }
            } catch (NumberFormatException e) {
                errors.rejectValue("amount", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                log.debug("QuantityDto.amount is invalid");
                return;
            }
        }

        Optional<String> optProductId = dto.getProductId();
        if(!fieldsToEscape.contains("productId") && optProductId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optProductId.get()))) {
            errors.rejectValue("productId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            log.debug("QuantityDto.productId is invalid");
            return;
        } else if(!fieldsToEscape.contains("productId") && optProductId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optProductId.get()))) {
            String productId = optProductId.get();
            try {
                Long.parseLong(productId);
            } catch (NumberFormatException e) {
                log.debug("QuantityDto.productId is invalid");
                errors.rejectValue("productId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return;
            }
            try {
                ProductVo productVo = productService.retrieveDetailsById(productId, Optional.of(TOABCascadeLevel.ONE));
                if(!productVo.getActive()) {
                    log.debug("QuantityDto.productId is inactive");
                    errors.rejectValue("productId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (ProductException e) {
                log.debug("QuantityDto.productId is invalid");
                errors.rejectValue("productId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                log.debug("QuantityDto.active is invalid");
                return;
            }
        }

        Optional<String> optWeightId = dto.getWeightId();
        if(!fieldsToEscape.contains("weightId") && optWeightId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optWeightId.get()))) {
            errors.rejectValue("weightId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            log.debug("QuantityDto.weightId is invalid");
            return;
        } else if(!fieldsToEscape.contains("weightId") && optWeightId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optWeightId.get()))) {
            if(!inventoryServiceHelper.isWeightCodeValid(optWeightId.get())) {
                log.debug("QuantityDto.weightId is invalid");
                errors.rejectValue("weightId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return;
            }
        }
    }

}
