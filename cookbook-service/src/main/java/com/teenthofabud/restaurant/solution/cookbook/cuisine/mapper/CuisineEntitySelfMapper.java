package com.teenthofabud.restaurant.solution.cookbook.cuisine.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class CuisineEntitySelfMapper implements SingleChannelMapper<CuisineEntity> {

    @Override
    public Optional<CuisineEntity> compareAndMap(CuisineEntity source, CuisineEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source CuisineEntity.id is valid");
        }
        if(source.getName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getName())) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source CuisineEntity.name is valid");
        }
        if(source.getDescription() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getDescription())) && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source CuisineEntity.description is valid");
        }
        if(changeSW) {
            log.debug("All provided CuisineEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided CuisineEntity attributes are valid");
            return Optional.empty();
        }
    }
}
