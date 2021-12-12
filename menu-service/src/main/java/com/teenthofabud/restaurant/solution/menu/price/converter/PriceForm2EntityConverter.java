package com.teenthofabud.restaurant.solution.menu.price.converter;

import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import com.teenthofabud.restaurant.solution.menu.item.repository.ItemRepository;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceEntity;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class PriceForm2EntityConverter implements Converter<PriceForm, PriceEntity> {

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
    public PriceEntity convert(PriceForm form) {
        PriceEntity entity = new PriceEntity();
        if(!fieldsToEscape.contains("amount")) {
            entity.setAmount(form.getAmount());
        }
        if(!fieldsToEscape.contains("currencyId")) {
            entity.setCurrencyId(form.getCurrencyId());
        }
        if(!fieldsToEscape.contains("itemId")) {
            Long itemId = Long.parseLong(form.getItemId());
            Optional<ItemEntity> itemEntity = itemRepository.findById(itemId);
            entity.setItem(itemEntity.get());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
