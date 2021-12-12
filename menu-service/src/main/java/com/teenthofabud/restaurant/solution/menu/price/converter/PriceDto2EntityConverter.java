package com.teenthofabud.restaurant.solution.menu.price.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import com.teenthofabud.restaurant.solution.menu.item.repository.ItemRepository;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceDto;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class PriceDto2EntityConverter implements ComparativePatchConverter<PriceDto, PriceEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 6;

    private List<String> fieldsToEscape;
    private ItemRepository itemRepository;

    @Autowired
    public void setItemRepository(ItemRepository itemRepository) {
        this.itemRepository = itemRepository;
    }
    
    @Value("#{'${res.menu.price.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public void compareAndMap(PriceDto dto, PriceEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optAmount = dto.getAmount();
        if(!fieldsToEscape.contains("amount") && optAmount.isPresent()) {
            actualEntity.setAmount(Double.parseDouble(optAmount.get()));
            changeSW[i++] = true;
            log.debug("PriceDto.amount is valid");
        }
        Optional<String> optItemId = dto.getItemId();
        if(!fieldsToEscape.contains("itemId") && optItemId.isPresent()) {
            Long itemId = Long.parseLong(optItemId.get());
            Optional<ItemEntity> itemEntity = itemRepository.findById(itemId);
            actualEntity.setItem(itemEntity.get());
            changeSW[i++] = true;
            log.debug("PriceDto.itemId is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("PriceDto.active is valid");
        }
        Optional<String> optCurrencyId = dto.getCurrencyId();
        if(!fieldsToEscape.contains("currencyId") && optCurrencyId.isPresent()) {
            actualEntity.setCurrencyId(optCurrencyId.get());
            changeSW[i++] = true;
            log.debug("PriceDto.currencyId is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided PriceDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided PriceDto attributes are valid");
    }

}
