package com.teenthofabud.restaurant.solution.establishmentarea.table.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.service.FloorService;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableDto;
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
public class TableDtoValidator implements Validator {

    private List<String> fieldsToEscape;
    private FloorService floorService;

    @Value("#{'${res.establishment.area.floor.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(TableDto.class);
    }

    @Autowired
    public void setFloorService(FloorService floorService) {
        this.floorService = floorService;
    }

    @Override
    public void validate(Object target, Errors errors) {
        TableDto dto = (TableDto) target;
        Optional<String> optTableName = dto.getTableName();
        if(!fieldsToEscape.contains("tableName") && optTableName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optTableName.get()))) {
            errors.rejectValue("tableName", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            log.debug("TableDto.tableName is invalid");
            return;
        }
        Optional<String> optDesc = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDesc.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optDesc.get()))) {
            errors.rejectValue("description", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            log.debug("TableDto.description is invalid");
            return;
        }
        Optional<String> optCapacity = dto.getCapacity();
        if(!fieldsToEscape.contains("capacity") && optCapacity.isPresent()) {
            if(StringUtils.hasText(StringUtils.trimWhitespace(optCapacity.get()))){
                try{
                    Long.valueOf(optCapacity.get());
                } catch (NumberFormatException e){
                    log.debug("TableForm.capacity is not a number");
                    errors.rejectValue("capacity", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
                    return;
                }
            }else{
                log.debug("TableDto.capacity is invalid");
                errors.rejectValue("capacity", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<String> optFloorId = dto.getFloorId();
        if(!fieldsToEscape.contains("floorId") && optFloorId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optFloorId.get()))) {
            String floorId = optFloorId.get();
            try {
                FloorVo floorVo = floorService.retrieveDetailsById(floorId, Optional.of(TOABCascadeLevel.ONE));
                if(!floorVo.getActive()) {
                    log.debug("TableDto.floorId is inactive");
                    errors.rejectValue("floorId", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (FloorException e) {
                log.debug("TableDto.floorId is invalid");
                errors.rejectValue("floorId", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
                log.debug("TableDto.active is invalid");
                return;
            }
        }
    }

}