package com.teenthofabud.restaurant.solution.establishmentarea.floor.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorDto;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class FloorDto2EntityConverter implements ComparativePatchConverter<FloorDto, FloorEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 2;

    private List<String> fieldsToEscape;

    @Value("#{'${res.establishment.area.floor.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public void compareAndMap(FloorDto dto, FloorEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optFloorName = dto.getFlrName();
        if(!fieldsToEscape.contains("flrName") && optFloorName.isPresent()) {
            actualEntity.setFlrName(optFloorName.get());
            changeSW[i++] = true;
            log.debug("FloorDto.flrName is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("FloorDto.active is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) == 2) {
            log.debug("All provided FloorDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided FloorDto attributes are valid");
    }

}
