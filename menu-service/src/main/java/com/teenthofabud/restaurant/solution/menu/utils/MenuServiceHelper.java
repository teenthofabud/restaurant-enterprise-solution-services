package com.teenthofabud.restaurant.solution.menu.utils;

import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.menu.category.converter.CategoryEntity2VoConverter;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import com.teenthofabud.restaurant.solution.menu.item.converter.ItemEntity2VoConverter;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemException;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemVo;
import com.teenthofabud.restaurant.solution.menu.price.converter.PriceEntity2VoConverter;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceEntity;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceException;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;

@Slf4j
@Component
public class MenuServiceHelper {

    private CategoryEntity2VoConverter categoryEntity2VoConverter;
    private ItemEntity2VoConverter itemEntity2VoConverter;
    private PriceEntity2VoConverter priceEntity2VoConverter;

    @Autowired
    public void setPriceEntity2VoConverter(PriceEntity2VoConverter priceEntity2VoConverter) {
        this.priceEntity2VoConverter = priceEntity2VoConverter;
    }

    @Autowired
    public void setCategoryEntity2VoConverter(CategoryEntity2VoConverter categoryEntity2VoConverter) {
        this.categoryEntity2VoConverter = categoryEntity2VoConverter;
    }

    @Autowired
    public void setItemEntity2VoConverter(ItemEntity2VoConverter itemEntity2VoConverter) {
        this.itemEntity2VoConverter = itemEntity2VoConverter;
    }

    public List<CategoryVo> categoryEntity2DetailedVo(List<? extends CategoryEntity> categoryEntityList) {
        List<CategoryVo> categoryDetailsList = new LinkedList<>();
        if(categoryEntityList != null && !categoryEntityList.isEmpty()) {
            for(CategoryEntity entity : categoryEntityList) {
                CategoryVo vo = this.categoryEntity2DetailedVo(entity);
                categoryDetailsList.add(vo);
            }
        }
        return categoryDetailsList;
    }

    public List<ItemVo> itemEntity2DetailedVo(List<? extends ItemEntity> itemEntityList) {
        List<ItemVo> itemDetailsList = new LinkedList<>();
        if(itemEntityList != null && !itemEntityList.isEmpty()) {
            for(ItemEntity entity : itemEntityList) {
                ItemVo vo = this.itemEntity2DetailedVo(entity);
                itemDetailsList.add(vo);
            }
        }
        return itemDetailsList;
    }

    public CategoryVo categoryEntity2DetailedVo(CategoryEntity categoryEntity) {
        if(categoryEntity != null) {
            CategoryVo vo = categoryEntity2VoConverter.convert(categoryEntity);
            log.debug("Converting {} to {}", categoryEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "category entity is null" });
    }

    public ItemVo itemEntity2DetailedVo(ItemEntity itemEntity) {
        if(itemEntity != null) {
            ItemVo vo = itemEntity2VoConverter.convert(itemEntity);
            log.debug("Converting {} to {}", itemEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "item entity is null" });
    }

    public boolean isCurrencyCodeValid(String currencyCode) {
        Set<Currency> currencies = Currency.getAvailableCurrencies();
        if(currencies == null || currencies.isEmpty()) {
            log.debug("no currencies available in the system");
            throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object [] { "no currencies available in the system" });
        }
        Optional<Currency> optionalCurrency = currencies.stream().filter(c -> c.getCurrencyCode().compareTo(currencyCode) == 0).findAny();
        return optionalCurrency.isPresent();
    }

    public List<PriceVo> priceEntity2DetailedVo(List<PriceEntity> priceEntityList) {
        List<PriceVo> priceDetailsList = new LinkedList<>();
        if(priceEntityList != null && !priceEntityList.isEmpty()) {
            for(PriceEntity entity : priceEntityList) {
                PriceVo vo = this.priceEntity2DetailedVo(entity);
                priceDetailsList.add(vo);
            }
        }
        return priceDetailsList;
    }

    public PriceVo priceEntity2DetailedVo(PriceEntity priceEntity) {
        if(priceEntity != null) {
            PriceVo vo = priceEntity2VoConverter.convert(priceEntity);
            log.debug("Converting {} to {}", priceEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "price entity is null" });
    }
}
