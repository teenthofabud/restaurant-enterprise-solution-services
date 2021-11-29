package com.teenthofabud.restaurant.solution.menu.item.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.menu.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemForm;
import com.teenthofabud.restaurant.solution.menu.item.data.VegeterianStatus;
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
public class ItemFormRelaxedValidator implements RelaxedValidator<ItemForm>  {

    private List<String> fieldsToEscape;
    private CategoryService categoryService;

    @Value("#{'${res.menu.category.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setCategoryService(CategoryService categoryService) {
        this.categoryService = categoryService;
    }

    @Override
    public Boolean validateLoosely(ItemForm form, Errors errors) {
        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("ItemForm.name is empty");
            return false;
        }
        log.debug("ItemForm.name is valid");

        if(!fieldsToEscape.contains("description") && form.getDescription() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            errors.rejectValue("description", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("ItemForm.description is empty");
            return false;
        }
        log.debug("ItemForm.description is valid");

        if(!fieldsToEscape.contains("imageUrl") && form.getImageUrl() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getImageUrl()))) {
            errors.rejectValue("imageUrl", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("ItemForm.imageUrl is empty");
            return false;
        }
        log.debug("ItemForm.imageUrl is valid");

        if(!fieldsToEscape.contains("isVegeterian") && form.getIsVegeterian() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getIsVegeterian()))) {
            log.debug("ItemForm.isVegeterian is empty");
            errors.rejectValue("isVegeterian", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("isVegeterian") && form.getIsVegeterian() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getCategoryId()))) {
            try {
                VegeterianStatus.valueOf(form.getIsVegeterian());
            } catch (IllegalArgumentException e) {
                log.debug("ItemForm.isVegeterian is invalid");
                errors.rejectValue("isVegeterian", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return false;
            }
        }

        if(!fieldsToEscape.contains("categoryId") && form.getCategoryId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCategoryId()))) {
            log.debug("ItemForm.categoryId is empty");
            errors.rejectValue("categoryId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("categoryId") && form.getCategoryId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getCategoryId()))) {
            String categoryId = form.getCategoryId();
            try {
                Long.parseLong(categoryId);
            } catch (NumberFormatException e) {
                log.debug("ItemForm.categoryId is invalid");
                errors.rejectValue("categoryId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return false;
            }
            try {
                CategoryVo categoryVo = categoryService.retrieveDetailsById(categoryId, Optional.of(TOABCascadeLevel.ONE));
                if(!categoryVo.getActive()) {
                    log.debug("ItemForm.categoryId is inactive");
                    errors.rejectValue("categoryId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                    return false;
                }
            } catch (CategoryException e) {
                log.debug("ItemForm.categoryId is invalid");
                errors.rejectValue("categoryId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        return true;
    }
}
