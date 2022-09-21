package com.teenthofabud.restaurant.solution.engagement.category.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.checkin.error.CheckInErrorCode;
import com.teenthofabud.restaurant.solution.engagement.category.data.CategoryForm;
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

    @Value("#{'${res.reservation.category.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Boolean validateLoosely(CategoryForm form, Errors errors) {
        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("CategoryForm.name is empty");
            return false;
        }
        log.debug("CategoryForm.name is valid");
        if(!fieldsToEscape.contains("description") && form.getDescription() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            errors.rejectValue("description", CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID.name());
            log.debug("CategoryForm.description is empty");
            return false;
        }
        log.debug("CategoryForm.description is valid");
        return true;
    }
}
