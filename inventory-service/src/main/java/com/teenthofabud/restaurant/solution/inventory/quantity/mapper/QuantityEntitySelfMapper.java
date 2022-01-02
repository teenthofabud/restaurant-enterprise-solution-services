package com.teenthofabud.restaurant.solution.inventory.quantity.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class QuantityEntitySelfMapper implements SingleChannelMapper<QuantityEntity> {

    @Override
    public Optional<QuantityEntity> compareAndMap(QuantityEntity source, QuantityEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source QuantityEntity.id is valid");
        }

        if(source.getAmount() != null && source.getAmount().compareTo(target.getAmount()) != 0) {
            target.setAmount(source.getAmount());
            changeSW = true;
            log.debug("Source QuantityEntity.amount is valid");
        }

        if(source.getWeightId() != null && source.getWeightId().compareTo(target.getWeightId()) != 0) {
            target.setWeightId(source.getWeightId());
            changeSW = true;
            log.debug("Source QuantityEntity.weightId is valid");
        }

        if(source.getProduct() != null && source.getProduct().compareTo(target.getProduct()) != 0) {
            target.setProduct(source.getProduct());
            changeSW = true;
            log.debug("Source QuantityEntity.product is valid");
        }

        if(changeSW) {
            log.debug("All provided QuantityEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided QuantityEntity attributes are valid");
            return Optional.empty();
        }
    }
}
