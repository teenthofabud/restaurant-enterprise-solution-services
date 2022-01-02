package com.teenthofabud.restaurant.solution.inventory.category.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class CategoryEntitySelfMapper implements SingleChannelMapper<CategoryEntity> {

    @Override
    public Optional<CategoryEntity> compareAndMap(CategoryEntity source, CategoryEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source CategoryEntity.id is valid");
        }
        if(source.getName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getName())) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source CategoryEntity.name is valid");
        }
        if(source.getDescription() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getDescription())) && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source CategoryEntity.description is valid");
        }
        if(changeSW) {
            log.debug("All provided CategoryEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided CategoryEntity attributes are valid");
            return Optional.empty();
        }
    }
}
