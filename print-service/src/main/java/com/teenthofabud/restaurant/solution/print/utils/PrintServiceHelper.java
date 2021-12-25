package com.teenthofabud.restaurant.solution.print.utils;

import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.print.template.converter.TemplateEntity2VoConverter;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateEntity;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;

@Slf4j
@Component
public class PrintServiceHelper {

    private TemplateEntity2VoConverter templateEntity2VoConverter;
    /*private ItemEntity2VoConverter itemEntity2VoConverter;
    private PriceEntity2VoConverter priceEntity2VoConverter;

    @Autowired
    public void setPriceEntity2VoConverter(PriceEntity2VoConverter priceEntity2VoConverter) {
        this.priceEntity2VoConverter = priceEntity2VoConverter;
    }*/

    @Autowired
    public void setTemplateEntity2VoConverter(TemplateEntity2VoConverter templateEntity2VoConverter) {
        this.templateEntity2VoConverter = templateEntity2VoConverter;
    }

    /*@Autowired
    public void setItemEntity2VoConverter(ItemEntity2VoConverter itemEntity2VoConverter) {
        this.itemEntity2VoConverter = itemEntity2VoConverter;
    }*/

    public List<TemplateVo> templateEntity2DetailedVo(List<? extends TemplateEntity> templateEntityList) {
        List<TemplateVo> templateDetailsList = new LinkedList<>();
        if(templateEntityList != null && !templateEntityList.isEmpty()) {
            for(TemplateEntity entity : templateEntityList) {
                TemplateVo vo = this.templateEntity2DetailedVo(entity);
                templateDetailsList.add(vo);
            }
        }
        return templateDetailsList;
    }

    public TemplateVo templateEntity2DetailedVo(TemplateEntity templateEntity) {
        if(templateEntity != null) {
            TemplateVo vo = templateEntity2VoConverter.convert(templateEntity);
            log.debug("Converting {} to {}", templateEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "template entity is null" });
    }

    /*public List<ItemVo> itemEntity2DetailedVo(List<? extends ItemEntity> itemEntityList) {
        List<ItemVo> itemDetailsList = new LinkedList<>();
        if(itemEntityList != null && !itemEntityList.isEmpty()) {
            for(ItemEntity entity : itemEntityList) {
                ItemVo vo = this.itemEntity2DetailedVo(entity);
                itemDetailsList.add(vo);
            }
        }
        return itemDetailsList;
    }

    public ItemVo itemEntity2DetailedVo(ItemEntity itemEntity) {
        if(itemEntity != null) {
            ItemVo vo = itemEntity2VoConverter.convert(itemEntity);
            log.debug("Converting {} to {}", itemEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "item entity is null" });
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
    }*/
}
