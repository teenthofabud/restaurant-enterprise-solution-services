package com.teenthofabud.restaurant.solution.inventory.product.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.inventory.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.inventory.error.InventoryErrorCode;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductDto;
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
public class ProductDtoValidator implements Validator {

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
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(ProductDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        ProductDto dto = (ProductDto) target;
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            log.debug("ProductDto.name is invalid");
            return;
        }

        Optional<String> optDescription = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDescription.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optDescription.get()))) {
            errors.rejectValue("description", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            log.debug("ProductDto.description is invalid");
            return;
        }

        Optional<String> optImageUrl = dto.getImageUrl();
        if(!fieldsToEscape.contains("imageUrl") && optImageUrl.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optImageUrl.get()))) {
            errors.rejectValue("imageUrl", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            log.debug("ProductDto.imageUrl is invalid");
            return;
        }

        Optional<String> optCategoryId = dto.getCategoryId();
        if(!fieldsToEscape.contains("categoryId") && optCategoryId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optCategoryId.get()))) {
            errors.rejectValue("categoryId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            log.debug("ProductDto.categoryId is invalid");
            return;
        } else if(!fieldsToEscape.contains("categoryId") && optCategoryId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optCategoryId.get()))) {
            String categoryId = optCategoryId.get();
            try {
                Long.parseLong(categoryId);
            } catch (NumberFormatException e) {
                log.debug("ProductDto.categoryId is invalid");
                errors.rejectValue("categoryId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return;
            }
            try {
                CategoryVo categoryVo = categoryService.retrieveDetailsById(categoryId, Optional.of(TOABCascadeLevel.ONE));
                if(!categoryVo.getActive()) {
                    log.debug("ProductDto.categoryId is inactive");
                    errors.rejectValue("categoryId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (CategoryException e) {
                log.debug("ProductDto.categoryId is invalid");
                errors.rejectValue("categoryId", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                return;
            }
        }

            Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                log.debug("ProductDto.active is invalid");
                return;
            }
        }
    }

}
