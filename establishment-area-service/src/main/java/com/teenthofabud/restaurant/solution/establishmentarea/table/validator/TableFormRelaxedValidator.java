package com.teenthofabud.restaurant.solution.establishmentarea.table.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.service.FloorService;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class TableFormRelaxedValidator implements RelaxedValidator<TableForm>  {

    private List<String> fieldsToEscape;
    private FloorService floorService;

    @Value("#{'${res.establishment.area.table.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setFloorService(FloorService floorService) {
        this.floorService = floorService;
    }

    @Override
    public Boolean validateLoosely(TableForm form, Errors errors) {
        if(!fieldsToEscape.contains("tableName") && form.getTableName() != null
                && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getTableName()))) {
            errors.rejectValue("tableName", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            log.debug("TableForm.tableName is empty");
            return false;
        }
        log.debug("TableForm.tableName is valid");
        
        if(!fieldsToEscape.contains("description") && form.getDescription() != null
                && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDescription()))) {
            errors.rejectValue("description", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            log.debug("TableForm.description is empty");
            return false;
        }
        log.debug("TableForm.description is valid");
        
        if(!fieldsToEscape.contains("capacity") && form.getCapacity() != null && form.getCapacity() <= 0) {
            errors.rejectValue("capacity", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            log.debug("TableForm.capacity is empty");
            return false;
        }
        log.debug("TableForm.capacity is valid");

        if(!fieldsToEscape.contains("floorId") && form.getFloorId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getFloorId()))) {
            errors.rejectValue("floorId", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            log.debug("TableForm.floorId is empty");
            return false;
        } else if(!fieldsToEscape.contains("floorId") && form.getFloorId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getFloorId()))){
            String floorId = form.getFloorId();
            try {
                FloorVo floorVo = floorService.retrieveDetailsById(floorId, Optional.of(TOABCascadeLevel.ONE));
                if(!floorVo.getActive()) {
                    log.debug("TableForm.floorId is inactive");
                    errors.rejectValue("floorId", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
                    return false;
                }
            } catch (FloorException e) {
                log.debug("TableForm.floorId is invalid");
                errors.rejectValue("floorId", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("TableForm.floorId is valid");
        
        return true;
    }
}