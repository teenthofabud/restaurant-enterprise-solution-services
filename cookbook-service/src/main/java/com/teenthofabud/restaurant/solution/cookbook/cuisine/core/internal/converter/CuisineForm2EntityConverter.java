package com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.converter;

import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.dto.CuisineRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class CuisineForm2EntityConverter implements Converter<CuisineRequest, CuisineEntity> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.cookbook.cuisine.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public CuisineEntity convert(CuisineRequest form) {
        CuisineEntity entity = new CuisineEntity();
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
