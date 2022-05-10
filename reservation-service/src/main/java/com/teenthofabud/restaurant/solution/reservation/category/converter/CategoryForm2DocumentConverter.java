package com.teenthofabud.restaurant.solution.reservation.category.converter;

import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryDocument;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class CategoryForm2DocumentConverter implements Converter<CategoryForm, CategoryDocument> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.reservation.category.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public CategoryDocument convert(CategoryForm form) {
        CategoryDocument entity = new CategoryDocument();
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
