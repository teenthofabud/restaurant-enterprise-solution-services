package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorForm;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;

@Component
@Slf4j
public class KitchenFormRelaxedValidator implements RelaxedValidator<KitchenForm>  {

    private List<String> fieldsToEscape;

    @Value("#{'${res.establishment.area.kitchen.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Boolean validateLoosely(KitchenForm form, Errors errors) {
        if(!fieldsToEscape.contains("kitchenName") && form.getKitchenName() != null
                && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getKitchenName()))) {
            errors.rejectValue("kitchenName", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            log.debug("KitchenForm.kitchenName is empty");
            return false;
        }
        log.debug("KitchenForm.kitchenName is valid");
        if(!fieldsToEscape.contains("description") && form.getDescription() != null
                && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            errors.rejectValue("description", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            log.debug("KitchenForm.description is empty");
            return false;
        }
        log.debug("KitchenForm.description is valid");
        return true;
    }
}