package com.teenthofabud.restaurant.solution.cookbook.cuisine.validator;

import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineForm;
import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;

@Component
@Slf4j
public class CuisineFormValidator implements Validator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.cookbook.cuisine.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(CuisineForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        CuisineForm form = (CuisineForm) target;
        if(!fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("CuisineForm.name is empty");
            errors.rejectValue("name", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("description") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            log.debug("CuisineForm.description is empty");
            errors.rejectValue("description", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return;
        }
    }

}
