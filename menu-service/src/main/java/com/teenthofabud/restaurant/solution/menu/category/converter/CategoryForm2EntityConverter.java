package com.teenthofabud.restaurant.solution.menu.category.converter;

import com.teenthofabud.restaurant.solution.menu.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class CategoryForm2EntityConverter implements Converter<CategoryForm, CategoryEntity> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.menu.category.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public CategoryEntity convert(CategoryForm form) {
        CategoryEntity entity = new CategoryEntity();
        if(!fieldsToEscape.contains("name")) {
            entity.setName(form.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            entity.setDescription(form.getDescription());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
