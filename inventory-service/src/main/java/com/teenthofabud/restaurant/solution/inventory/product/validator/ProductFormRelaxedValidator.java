package com.teenthofabud.restaurant.solution.inventory.product.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.validator.RelaxedValidator;
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

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class ProductFormRelaxedValidator implements RelaxedValidator<ProductForm>  {

    private List<String> fieldsToEscape;
    private CategoryService categoryService;

    @Value("#{'${res.inventory.product.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setCategoryService(CategoryService categoryService) {
        this.categoryService = categoryService;
    }

    @Override
    public Boolean validateLoosely(ProductForm form, Errors errors) {
        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            log.debug("ProductForm.name is empty");
            return false;
        }
        log.debug("ProductForm.name is valid");

        if(!fieldsToEscape.contains("description") && form.getDescription() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            errors.rejectValue("description", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            log.debug("ProductForm.description is empty");
            return false;
        }
        log.debug("ProductForm.description is valid");

        if(!fieldsToEscape.contains("imageUrl") && form.getImageUrl() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getImageUrl()))) {
            errors.rejectValue("imageUrl", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            log.debug("ProductForm.imageUrl is empty");
            return false;
        }
        log.debug("ProductForm.imageUrl is valid");

        if(!fieldsToEscape.contains("categoryId") && form.getCategoryId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCategoryId()))) {
            log.debug("ProductForm.categoryId is empty");
            errors.rejectValue("categoryId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("categoryId") && form.getCategoryId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getCategoryId()))) {
            String categoryId = form.getCategoryId();
            try {
                Long.parseLong(categoryId);
            } catch (NumberFormatException e) {
                log.debug("ProductForm.categoryId is invalid");
                errors.rejectValue("categoryId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return false;
            }
            try {
                CategoryVo categoryVo = categoryService.retrieveDetailsById(categoryId, Optional.of(TOABCascadeLevel.ONE));
                if(!categoryVo.getActive()) {
                    log.debug("ProductForm.categoryId is inactive");
                    errors.rejectValue("categoryId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                    return false;
                }
            } catch (CategoryException e) {
                log.debug("ProductForm.categoryId is invalid");
                errors.rejectValue("categoryId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        return true;
    }
}
