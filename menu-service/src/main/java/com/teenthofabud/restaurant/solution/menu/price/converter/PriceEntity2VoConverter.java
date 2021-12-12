package com.teenthofabud.restaurant.solution.menu.price.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemVo;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceEntity;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceVo;
import com.teenthofabud.restaurant.solution.menu.utils.MenuServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.scheduling.concurrent.CustomizableThreadFactory;
import org.springframework.stereotype.Component;

import java.util.Currency;
import java.util.List;
import java.util.concurrent.*;

@Component
@Slf4j
public class PriceEntity2VoConverter extends TOABBaseEntity2VoConverter<PriceEntity, PriceVo> implements Converter<PriceEntity, PriceVo> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.menu.price.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private MenuServiceHelper menuServiceHelper;

    @Autowired
    public void setMenuServiceHelper(MenuServiceHelper menuServiceHelper) {
        this.menuServiceHelper = menuServiceHelper;
    }

    @Override
    public PriceVo convert(PriceEntity entity) {
        PriceVo vo = new PriceVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("amount")) {
            vo.setAmount(entity.getAmount());
        }
        if(!fieldsToEscape.contains("currencyId")) {
            expandSecondLevelFields(entity, vo, "currencyId");
        }
        if(!fieldsToEscape.contains("itemId")) {
            expandSecondLevelFields(entity, vo, "itemId");
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

    private void expandSecondLevelFields(PriceEntity entity, PriceVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("itemId") && fieldName.compareTo("itemId") == 0) {
                    Callable<ItemVo> itemEntity2VoConversion = () -> {
                        TOABRequestContextHolder.setCascadeLevelContext(TOABCascadeLevel.ZERO);
                        ItemVo ItemVo = menuServiceHelper.itemEntity2DetailedVo(entity.getItem());
                        TOABRequestContextHolder.clearCascadeLevelContext();
                        return ItemVo;
                    };
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory("itemEntity2VoConversion-"));
                    Future<ItemVo> itemEntity2VoConversionResult = executorService.submit(itemEntity2VoConversion);
                    try {
                        ItemVo ItemVo = itemEntity2VoConversionResult.get();
                        vo.setItem(ItemVo);
                        log.debug("Retrieved {} for ItemId: {}", ItemVo, entity.getItem().getId());
                    } catch (InterruptedException | ExecutionException e) {
                        log.error("Unable to perform itemEntity2VoConversion", e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "Unable to perform itemEntity2VoConversion",
                                new Object[] { "itemEntity2VoConversion failure: " + e.getMessage() });
                    }
                }
                if(!fieldsToEscape.contains("currencyId") && fieldName.compareTo("currencyId") == 0) {
                    vo.setCurrency(Currency.getInstance(entity.getCurrencyId()));
                }
                break;
            default:
                vo.setItemId(entity.getItem().getId().toString());
                vo.setCurrencyId(entity.getCurrencyId());
                log.debug("only first level cascaded for ItemId");
                break;
        }
    }

}
