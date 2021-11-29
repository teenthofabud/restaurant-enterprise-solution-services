package com.teenthofabud.restaurant.solution.menu.item.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.menu.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemDto;
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
public class ItemDtoValidator implements Validator {

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
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(ItemDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        ItemDto dto = (ItemDto) target;
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("ItemDto.name is invalid");
            return;
        }

        Optional<String> optDescription = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDescription.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optDescription.get()))) {
            errors.rejectValue("description", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("ItemDto.description is invalid");
            return;
        }

        Optional<String> optImageUrl = dto.getImageUrl();
        if(!fieldsToEscape.contains("imageUrl") && optImageUrl.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optImageUrl.get()))) {
            errors.rejectValue("imageUrl", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("ItemDto.imageUrl is invalid");
            return;
        }

        Optional<String> optCategoryId = dto.getCategoryId();
        if(!fieldsToEscape.contains("categoryId") && optCategoryId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optCategoryId.get()))) {
            errors.rejectValue("categoryId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("ItemDto.categoryId is invalid");
            return;
        } else if(!fieldsToEscape.contains("categoryId") && optCategoryId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optCategoryId.get()))) {
            String categoryId = optCategoryId.get();
            try {
                Long.parseLong(categoryId);
            } catch (NumberFormatException e) {
                log.debug("ItemDto.categoryId is invalid");
                errors.rejectValue("categoryId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return;
            }
            try {
                CategoryVo categoryVo = categoryService.retrieveDetailsById(categoryId, Optional.of(TOABCascadeLevel.ONE));
                if(!categoryVo.getActive()) {
                    log.debug("ItemDto.categoryId is inactive");
                    errors.rejectValue("categoryId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (CategoryException e) {
                log.debug("ItemDto.categoryId is invalid");
                errors.rejectValue("categoryId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return;
            }
        }

            Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                log.debug("ItemDto.active is invalid");
                return;
            }
        }

        Optional<String> optIsVegeterian = dto.getIsVegeterian();
        if(!fieldsToEscape.contains("isVegeterian") && optIsVegeterian.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optIsVegeterian.get()))) {
            errors.rejectValue("isVegeterian", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("ItemDto.isVegeterian is invalid");
            return;
        } else if(!fieldsToEscape.contains("isVegeterian") && optIsVegeterian.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optIsVegeterian.get()))) {
            String isVegeterian = optIsVegeterian.get();
            try {
                VegeterianStatus.valueOf(isVegeterian);
            } catch (IllegalArgumentException e) {
                log.debug("ItemDto.isVegeterian is invalid");
                errors.rejectValue("isVegeterian", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return;
            }
        }
    }

}
