package com.teenthofabud.restaurant.solution.establishmentarea.floor.validator;

import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class FloorDtoValidator implements Validator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.establishment.area.floor.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(FloorDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        FloorDto dto = (FloorDto) target;
        Optional<String> optFirstName = dto.getFlrName();
        if(!fieldsToEscape.contains("flrName") && optFirstName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optFirstName.get()))) {
            errors.rejectValue("flrName", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
            log.debug("FloorDto.flrName is invalid");
            return;
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.name());
                log.debug("FloorDto.active is invalid");
                return;
            }
        }
    }

}