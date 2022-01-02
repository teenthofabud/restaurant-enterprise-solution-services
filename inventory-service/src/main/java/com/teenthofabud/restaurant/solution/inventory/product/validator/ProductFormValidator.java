package com.teenthofabud.restaurant.solution.inventory.product.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.inventory.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.inventory.error.InventoryErrorCode;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductForm;
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
public class ProductFormValidator implements Validator {

    private List<String> fieldsToEscape;

    private CategoryService categoryService;

    @Autowired
    public void setCategoryService(CategoryService categoryService) {
        this.categoryService = categoryService;
    }

    @Value("#{'${res.inventory.product.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }
    
    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(ProductForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        ProductForm form = (ProductForm) target;
        if(!fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("ProductForm.name is empty");
            errors.rejectValue("name", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("imageUrl") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getImageUrl()))) {
            log.debug("ProductForm.imageUrl is empty");
            errors.rejectValue("imageUrl", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("categoryId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCategoryId()))) {
            log.debug("ProductForm.categoryId is empty");
            errors.rejectValue("categoryId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("categoryId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCategoryId()))) {
            String categoryId = form.getCategoryId();
            try {
                Long.parseLong(categoryId);
            } catch (NumberFormatException e) {
                log.debug("ProductForm.categoryId is invalid");
                errors.rejectValue("categoryId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return;
            }
            try {
                CategoryVo categoryVo = categoryService.retrieveDetailsById(categoryId, Optional.of(TOABCascadeLevel.ONE));
                if(!categoryVo.getActive()) {
                    log.debug("ProductForm.categoryId is inactive");
                    errors.rejectValue("categoryId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (CategoryException e) {
                log.debug("ProductForm.categoryId is invalid");
                errors.rejectValue("categoryId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return;
            }
        }
    }

}
