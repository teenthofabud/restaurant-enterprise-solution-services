package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.service.FloorService;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenDto;
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
public class KitchenDtoValidator implements Validator {

    private List<String> fieldsToEscape;
    private FloorService floorService;

    @Value("#{'${res.establishment.area.floor.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(KitchenDto.class);
    }

    @Autowired
    public void setFloorService(FloorService floorService) {
        this.floorService = floorService;
    }

    @Override
    public void validate(Object target, Errors errors) {
        KitchenDto dto = (KitchenDto) target;
        Optional<String> optKitchenName = dto.getKitchenName();
        if(!fieldsToEscape.contains("name") && optKitchenName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optKitchenName.get()))) {
            errors.rejectValue("name", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            log.debug("KitchenDto.name is invalid");
            return;
        }
        Optional<String> optDesc = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDesc.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optDesc.get()))) {
            errors.rejectValue("description", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            log.debug("KitchenDto.description is invalid");
            return;
        }
        Optional<String> optFloorId = dto.getFloorId();
        if(!fieldsToEscape.contains("floorId") && optFloorId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optFloorId.get()))) {
            String kitchenId = optFloorId.get();
            try {
                FloorVo floorVo = floorService.retrieveDetailsById(kitchenId, Optional.of(TOABCascadeLevel.ONE));
                if(!floorVo.getActive()) {
                    log.debug("KitchenDto.floorId is inactive");
                    errors.rejectValue("floorId", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (FloorException e) {
                log.debug("KitchenDto.floorId is invalid");
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
                log.debug("KitchenDto.active is invalid");
                return;
            }
        }
    }

}