package com.teenthofabud.restaurant.solution.menu.price.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import com.teenthofabud.restaurant.solution.menu.item.repository.ItemRepository;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceEntity;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class PriceForm2EntityMapper implements DualChannelMapper<PriceEntity, PriceForm> {

    private List<String> fieldsToEscape;
    private ItemRepository itemRepository;

    @Autowired
    public void setItemRepository(ItemRepository itemRepository) {
        this.itemRepository = itemRepository;
    }

    @Value("#{'${res.menu.item.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<PriceEntity> compareAndMap(PriceEntity actualEntity, PriceForm form) {
        PriceEntity expectedEntity = new PriceEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying PriceEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying PriceEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying PriceEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("amount") && form.getAmount() != null && form.getAmount().compareTo(actualEntity.getAmount()) != 0) {
            expectedEntity.setAmount(form.getAmount());
            changeSW = true;
            log.debug("PriceForm.name: {} is different as PriceEntity.name: {}", form.getAmount(), actualEntity.getAmount());
        } else {
            expectedEntity.setAmount(actualEntity.getAmount());
            log.debug("PriceForm.name: is unchanged");
        }

        if(!fieldsToEscape.contains("itemId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getItemId()))
                && form.getItemId().compareTo(actualEntity.getCurrencyId()) != 0) {
            Long itemId = Long.parseLong(form.getItemId());
            Optional<ItemEntity> optionalItemEntity = itemRepository.findById(itemId);
            if(actualEntity.getItem().compareTo(optionalItemEntity.get()) != 0) {
                expectedEntity.setItem(optionalItemEntity.get());
                changeSW = true;
                log.debug("PriceForm.itemId: {} is different as PriceForm.itemId: {}", form.getItemId(), actualEntity.getItem().getId());
            } else {
                expectedEntity.setItem(actualEntity.getItem());
                log.debug("PriceForm.itemId: is unchanged");
            }
        } else {
            expectedEntity.setItem(actualEntity.getItem());
            log.debug("PriceForm.itemId: is unchanged");
        }

        if(!fieldsToEscape.contains("currencyId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCurrencyId()))
                && form.getCurrencyId().compareTo(actualEntity.getCurrencyId()) != 0) {
            expectedEntity.setCurrencyId(form.getCurrencyId());
            changeSW = true;
            log.debug("PriceForm.currencyId: {} is different as PriceEntity.currencyId: {}", form.getCurrencyId(), actualEntity.getCurrencyId());
        } else {
            expectedEntity.setCurrencyId(actualEntity.getCurrencyId());
            log.debug("PriceForm.currencyId: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
