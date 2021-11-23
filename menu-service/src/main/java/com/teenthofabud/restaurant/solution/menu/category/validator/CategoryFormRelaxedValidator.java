package com.teenthofabud.restaurant.solution.menu.category.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryForm;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;

@Component
@Slf4j
public class CategoryFormRelaxedValidator implements RelaxedValidator<CategoryForm>  {

    private List<String> fieldsToEscape;

    @Value("#{'${res.menu.category.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Boolean validateLoosely(CategoryForm form, Errors errors) {
        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("CategoryForm.name is empty");
            return false;
        }
        log.debug("CategoryForm.name is valid");
        if(!fieldsToEscape.contains("description") && form.getDescription() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            errors.rejectValue("description", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("CategoryForm.description is empty");
            return false;
        }
        log.debug("CategoryForm.description is valid");
        return true;
    }
}
