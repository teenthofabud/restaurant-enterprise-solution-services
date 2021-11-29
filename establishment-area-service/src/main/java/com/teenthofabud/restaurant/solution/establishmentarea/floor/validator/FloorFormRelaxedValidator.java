package com.teenthofabud.restaurant.solution.establishmentarea.floor.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;

@Component
@Slf4j
public class FloorFormRelaxedValidator implements RelaxedValidator<FloorForm>  {

    private List<String> fieldsToEscape;

    @Value("#{'${res.establishment.area.floor.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Boolean validateLoosely(FloorForm form, Errors errors) {
        if(!fieldsToEscape.contains("flrName") && form.getFlrName() != null
                && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getFlrName()))) {
            errors.rejectValue("flrName", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            log.debug("FloorForm.flrName is empty");
            return false;
        }
        log.debug("FloorForm.flrName is valid");
        return true;
    }
}