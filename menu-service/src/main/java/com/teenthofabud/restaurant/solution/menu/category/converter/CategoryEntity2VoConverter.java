package com.teenthofabud.restaurant.solution.menu.category.converter;

import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class CategoryEntity2VoConverter extends TOABBaseEntity2VoConverter<CategoryEntity, CategoryVo> implements Converter<CategoryEntity, CategoryVo> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.menu.category.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public CategoryVo convert(CategoryEntity entity) {
        CategoryVo vo = new CategoryVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

}
