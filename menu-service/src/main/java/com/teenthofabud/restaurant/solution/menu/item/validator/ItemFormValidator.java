package com.teenthofabud.restaurant.solution.menu.item.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemForm;
import com.teenthofabud.restaurant.solution.menu.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.menu.item.data.VegeterianStatus;
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
public class ItemFormValidator implements Validator {

    private List<String> fieldsToEscape;

    private CategoryService categoryService;

    @Autowired
    public void setCategoryService(CategoryService categoryService) {
        this.categoryService = categoryService;
    }

    @Value("#{'${res.menu.item.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }
    
    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(ItemForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        ItemForm form = (ItemForm) target;
        if(!fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("ItemForm.name is empty");
            errors.rejectValue("name", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("isVegeterian") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getIsVegeterian()))) {
            log.debug("ItemForm.isVegeterian is empty");
            errors.rejectValue("isVegeterian", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("isVegeterian") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCategoryId()))) {
            try {
                VegeterianStatus.valueOf(form.getIsVegeterian());
            } catch (IllegalArgumentException e) {
                log.debug("ItemForm.isVegeterian is invalid");
                errors.rejectValue("isVegeterian", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        if(!fieldsToEscape.contains("imageUrl") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getImageUrl()))) {
            log.debug("ItemForm.imageUrl is empty");
            errors.rejectValue("imageUrl", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("categoryId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCategoryId()))) {
            log.debug("ItemForm.categoryId is empty");
            errors.rejectValue("categoryId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("categoryId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCategoryId()))) {
            String categoryId = form.getCategoryId();
            try {
                Long.parseLong(categoryId);
            } catch (NumberFormatException e) {
                log.debug("ItemForm.categoryId is invalid");
                errors.rejectValue("categoryId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return;
            }
            try {
                CategoryVo categoryVo = categoryService.retrieveDetailsById(categoryId, Optional.of(TOABCascadeLevel.ONE));
                if(!categoryVo.getActive()) {
                    log.debug("ItemForm.categoryId is inactive");
                    errors.rejectValue("categoryId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (CategoryException e) {
                log.debug("ItemForm.categoryId is invalid");
                errors.rejectValue("categoryId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return;
            }
        }
    }

}
