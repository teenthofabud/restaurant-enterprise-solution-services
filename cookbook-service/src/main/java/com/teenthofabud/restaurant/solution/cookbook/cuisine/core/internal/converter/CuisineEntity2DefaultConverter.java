package com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.converter;

import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.entities.Cuisine;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class CuisineEntity2DefaultConverter implements Converter<CuisineEntity, Cuisine> {

    private List<String> fieldsToEscape;
    //private CookbookServiceHelper cookbookServiceHelper;

    @Value("#{'${res.cookbook.cuisine.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    /*@Autowired
    public void setCuisineServiceHelper(CookbookServiceHelper cookbookServiceHelper) {
        this.cookbookServiceHelper = cookbookServiceHelper;
    }*/

    @Override
    public Cuisine convert(CuisineEntity entity) {
        Cuisine vo = new Cuisine();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        vo.setCreatedOn(entity.getCreatedOn());
        vo.setCreatedBy(entity.getCreatedBy());
        vo.setModifiedOn(entity.getModifiedOn());
        vo.setModifiedBy(entity.getModifiedBy());
        vo.setActive(entity.getActive());
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

}
