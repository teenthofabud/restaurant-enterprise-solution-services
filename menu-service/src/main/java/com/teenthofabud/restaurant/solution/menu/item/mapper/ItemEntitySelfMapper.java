package com.teenthofabud.restaurant.solution.menu.item.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class ItemEntitySelfMapper implements SingleChannelMapper<ItemEntity> {

    @Override
    public Optional<ItemEntity> compareAndMap(ItemEntity source, ItemEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source ItemEntity.id is valid");
        }

        if(source.getName() != null && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source ItemEntity.name is valid");
        }

        if(source.getDescription() != null && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source ItemEntity.description is valid");
        }

        if(source.getIsVegeterian() != null && source.getIsVegeterian().compareTo(target.getIsVegeterian()) != 0) {
            target.setIsVegeterian(source.getIsVegeterian());
            changeSW = true;
            log.debug("Source ItemEntity.isVegeterian is valid");
        }

        if(source.getImageUrl() != null && source.getImageUrl().compareTo(target.getImageUrl()) != 0) {
            target.setImageUrl(source.getImageUrl());
            changeSW = true;
            log.debug("Source ItemEntity.imageUrl is valid");
        }

        if(source.getCategory() != null && source.getCategory().compareTo(target.getCategory()) != 0) {
            target.setCategory(source.getCategory());
            changeSW = true;
            log.debug("Source ItemEntity.category is valid");
        }

        if(changeSW) {
            log.debug("All provided ItemEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided ItemEntity attributes are valid");
            return Optional.empty();
        }
    }
}
