package com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.dto.CuisineResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class CuisineEntity2VoConverter extends TOABBaseEntity2VoConverter<CuisineEntity, CuisineResponse> implements Converter<CuisineEntity, CuisineResponse> {

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
    public CuisineResponse convert(CuisineEntity entity) {
        CuisineResponse vo = new CuisineResponse();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        if(!fieldsToEscape.contains("recipes")) {
            this.expandSecondLevelFields(entity, vo, "recipes");
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

    private void expandSecondLevelFields(CuisineEntity entity, CuisineResponse vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                break;
            default:
                break;
        }
    }

}
