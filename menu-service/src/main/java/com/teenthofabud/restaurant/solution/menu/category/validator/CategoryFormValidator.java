package com.teenthofabud.restaurant.solution.menu.category.validator;

import com.teenthofabud.restaurant.solution.menu.category.data.CategoryForm;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;

@Component
@Slf4j
public class CategoryFormValidator implements Validator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.menu.category.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }
    
    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(CategoryForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        CategoryForm form = (CategoryForm) target;
        if(!fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("CategoryForm.name is empty");
            errors.rejectValue("name", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            return;
        }
    }

}
