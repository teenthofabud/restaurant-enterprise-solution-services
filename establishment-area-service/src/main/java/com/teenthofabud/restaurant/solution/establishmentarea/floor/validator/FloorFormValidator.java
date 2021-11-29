package com.teenthofabud.restaurant.solution.establishmentarea.floor.validator;

import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;

@Component
@Slf4j
public class FloorFormValidator implements Validator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.establishment.area.floor.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(FloorForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        FloorForm form = (FloorForm) target;
        if (!fieldsToEscape.contains("flrName") &&
                StringUtils.isEmpty(StringUtils.trimWhitespace(form.getFlrName()))) {
            log.debug("FloorForm.flrName is empty");
            errors.rejectValue("flrName", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            return;
        }
    }
}
