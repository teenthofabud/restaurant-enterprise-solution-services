package com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineForm;
import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;

@Component
@Slf4j
public class CuisineFormRelaxedValidator implements RelaxedValidator<CuisineForm>  {

    private List<String> fieldsToEscape;

    @Value("#{'${res.cookbook.cuisine.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Boolean validateLoosely(CuisineForm form, Errors errors) {
        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("CuisineForm.name is empty");
            return false;
        }
        log.debug("CuisineForm.name is valid");
        if(!fieldsToEscape.contains("description") && form.getDescription() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            errors.rejectValue("description", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("CuisineForm.description is empty");
            return false;
        }
        return true;
    }
}
