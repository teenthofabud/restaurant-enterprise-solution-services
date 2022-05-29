package com.teenthofabud.restaurant.solution.reservation.category.converter;

import com.teenthofabud.core.common.converter.TOABBaseDocument2VoConverter;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryDocument;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.reservation.utils.ReservationServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class CategoryDocument2VoConverter extends TOABBaseDocument2VoConverter<CategoryDocument, CategoryVo> implements Converter<CategoryDocument, CategoryVo> {

    private List<String> fieldsToEscape;
    private ReservationServiceHelper reservationServiceHelper;

    @Value("#{'${res.reservation.category.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }


    @Autowired
    public void setSessionServiceHelper(ReservationServiceHelper reservationServiceHelper) {
        this.reservationServiceHelper = reservationServiceHelper;
    }

    @Override
    public CategoryVo convert(CategoryDocument entity) {
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