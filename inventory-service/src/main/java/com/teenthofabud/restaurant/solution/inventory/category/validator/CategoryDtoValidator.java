package com.teenthofabud.restaurant.solution.inventory.category.validator;

import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryDto;
import com.teenthofabud.restaurant.solution.inventory.error.InventoryErrorCode;
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
public class CategoryDtoValidator implements Validator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.inventory.category.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(CategoryDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        CategoryDto dto = (CategoryDto) target;
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            log.debug("CategoryDto.name is invalid");
            return;
        }
        Optional<String> optDescription = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDescription.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optDescription.get()))) {
            errors.rejectValue("description", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
            log.debug("CategoryDto.description is invalid");
            return;
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.name());
                log.debug("CategoryDto.active is invalid");
                return;
            }
        }
    }

}
