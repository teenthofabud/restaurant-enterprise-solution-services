package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorForm;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.service.FloorService;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenForm;
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
public class KitchenFormValidator implements Validator {

    private List<String> fieldsToEscape;
    private FloorService floorService;

    @Value("#{'${res.establishment.area.kitchen.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setFloorService(FloorService floorService) {
        this.floorService = floorService;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(KitchenForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        KitchenForm form = (KitchenForm) target;
        if (!fieldsToEscape.contains("kitchenName") &&
                StringUtils.isEmpty(StringUtils.trimWhitespace(form.getKitchenName()))) {
            log.debug("KitchenForm.kitchenName is empty");
            errors.rejectValue("kitchenName", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            return;
        }

        if (!fieldsToEscape.contains("description") &&
                StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            log.debug("KitchenForm.description is empty");
            errors.rejectValue("description", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            return;
        }

        if(!fieldsToEscape.contains("floorId") && form.getFloorId() != null &&
                StringUtils.isEmpty(StringUtils.trimWhitespace(form.getFloorId()))) {
            errors.rejectValue("floorId", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            log.debug("TableForm.floorId is empty");
            return;
        } else if(!fieldsToEscape.contains("floorId") && form.getFloorId() != null &&
                StringUtils.hasText(StringUtils.trimWhitespace(form.getFloorId()))){
            String floorId = form.getFloorId();
            try {
                FloorVo floorVo = floorService.retrieveDetailsById(floorId, Optional.of(TOABCascadeLevel.ONE));
                if(!floorVo.getActive()) {
                    log.debug("TableForm.floorId is inactive");
                    errors.rejectValue("floorId", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (FloorException e) {
                log.debug("TableForm.floorId is invalid");
                errors.rejectValue("floorId", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("TableForm.floorId is valid");
    }
}
