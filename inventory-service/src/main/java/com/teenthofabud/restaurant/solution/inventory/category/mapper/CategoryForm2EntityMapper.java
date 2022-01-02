package com.teenthofabud.restaurant.solution.inventory.category.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class CategoryForm2EntityMapper implements DualChannelMapper<CategoryEntity, CategoryForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.inventory.category.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<CategoryEntity> compareAndMap(CategoryEntity actualEntity, CategoryForm form) {
        CategoryEntity expectedEntity = new CategoryEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying CategoryEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying CategoryEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying CategoryEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName()))
                && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("CategoryForm.name: {} is different as CategoryEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("CategoryForm.name: is unchanged");
        }
        if(!fieldsToEscape.contains("description") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription()))
                && form.getDescription().compareTo(actualEntity.getDescription()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("CategoryForm.description: {} is different as CategoryEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            log.debug("CategoryForm.description: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
