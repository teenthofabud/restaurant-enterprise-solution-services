package com.teenthofabud.restaurant.solution.inventory.product.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class ProductEntitySelfMapper implements SingleChannelMapper<ProductEntity> {

    @Override
    public Optional<ProductEntity> compareAndMap(ProductEntity source, ProductEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source ProductEntity.id is valid");
        }

        if(source.getName() != null && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source ProductEntity.name is valid");
        }

        if(source.getDescription() != null && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source ProductEntity.description is valid");
        }

        if(source.getImageUrl() != null && source.getImageUrl().compareTo(target.getImageUrl()) != 0) {
            target.setImageUrl(source.getImageUrl());
            changeSW = true;
            log.debug("Source ProductEntity.imageUrl is valid");
        }

        if(source.getCategory() != null && source.getCategory().compareTo(target.getCategory()) != 0) {
            target.setCategory(source.getCategory());
            changeSW = true;
            log.debug("Source ProductEntity.category is valid");
        }

        if(changeSW) {
            log.debug("All provided ProductEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided ProductEntity attributes are valid");
            return Optional.empty();
        }
    }
}
