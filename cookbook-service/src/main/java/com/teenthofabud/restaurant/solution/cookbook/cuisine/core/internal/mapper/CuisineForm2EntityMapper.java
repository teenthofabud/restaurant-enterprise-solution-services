package com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.dto.CuisineRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class CuisineForm2EntityMapper implements DualChannelMapper<CuisineEntity, CuisineRequest> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.cookbook.cuisine.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<CuisineEntity> compareAndMap(CuisineEntity actualEntity, CuisineRequest form) {
        CuisineEntity expectedEntity = new CuisineEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying CuisineEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying CuisineEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying CuisineEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName()))
                && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("CuisineRequest.name: {} is different as CuisineEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("CuisineRequest.name: is unchanged");
        }
        if(!fieldsToEscape.contains("description") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription()))
                && form.getDescription().compareTo(actualEntity.getDescription()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("CuisineRequest.description: {} is different as CuisineEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            log.debug("CuisineRequest.description: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
