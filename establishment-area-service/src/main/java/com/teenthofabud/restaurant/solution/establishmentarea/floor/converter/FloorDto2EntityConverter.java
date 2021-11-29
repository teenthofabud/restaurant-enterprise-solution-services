package com.teenthofabud.restaurant.solution.establishmentarea.floor.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorDto;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class FloorDto2EntityConverter implements ComparativePatchConverter<FloorDto, FloorEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 8;

    private List<String> fieldsToEscape;

    @Value("#{'${res.establishment.area.floor.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public void compareAndMap(FloorDto dto, FloorEntity actualEntity) throws TOABBaseException {

    }

}
