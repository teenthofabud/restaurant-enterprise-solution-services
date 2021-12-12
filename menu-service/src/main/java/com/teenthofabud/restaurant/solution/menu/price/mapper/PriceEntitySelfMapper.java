package com.teenthofabud.restaurant.solution.menu.price.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class PriceEntitySelfMapper implements SingleChannelMapper<PriceEntity> {

    @Override
    public Optional<PriceEntity> compareAndMap(PriceEntity source, PriceEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source PriceEntity.id is valid");
        }

        if(source.getAmount() != null && source.getAmount().compareTo(target.getAmount()) != 0) {
            target.setAmount(source.getAmount());
            changeSW = true;
            log.debug("Source PriceEntity.amount is valid");
        }

        if(source.getCurrencyId() != null && source.getCurrencyId().compareTo(target.getCurrencyId()) != 0) {
            target.setCurrencyId(source.getCurrencyId());
            changeSW = true;
            log.debug("Source PriceEntity.currencyId is valid");
        }

        if(source.getItem() != null && source.getItem().compareTo(target.getItem()) != 0) {
            target.setItem(source.getItem());
            changeSW = true;
            log.debug("Source PriceEntity.item is valid");
        }

        if(changeSW) {
            log.debug("All provided PriceEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided PriceEntity attributes are valid");
            return Optional.empty();
        }
    }
}
