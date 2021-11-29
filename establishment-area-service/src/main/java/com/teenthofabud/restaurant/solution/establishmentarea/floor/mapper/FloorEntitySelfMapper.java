package com.teenthofabud.restaurant.solution.establishmentarea.floor.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class FloorEntitySelfMapper implements SingleChannelMapper<FloorEntity> {

    @Override
    public Optional<FloorEntity> compareAndMap(FloorEntity source, FloorEntity target) {
        boolean changeSW = false;
        if(source.getFlrId() != null && source.getFlrId().compareTo(target.getFlrId()) != 0) {
            target.setFlrId(source.getFlrId());
            changeSW = true;
            log.debug("Source FloorEntity.flrId is valid");
        }if(source.getFlrName() != null &&
                StringUtils.hasText(StringUtils.trimWhitespace(source.getFlrName())) && source.getFlrName().compareTo(target.getFlrName()) != 0) {
            target.setFlrName(source.getFlrName());
            changeSW = true;
            log.debug("Source FloorEntity.flrName is valid");
        }
        if(changeSW) {
            log.debug("All provided FloorEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided FloorEntity attributes are valid");
            return Optional.empty();
        }
    }
}
