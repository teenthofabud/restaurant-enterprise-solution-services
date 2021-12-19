package com.teenthofabud.restaurant.solution.settings.utils;

import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.converter.PaymentMethodDocument2VoConverter;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodDocument;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodException;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;

@Slf4j
@Component
public class SettingsServiceHelper {

    private PaymentMethodDocument2VoConverter paymentMethodDocument2VoConverter;
    //private ItemDocument2VoConverter itemDocument2VoConverter;
    //private PriceDocument2VoConverter priceDocument2VoConverter;

    /*@Autowired
    public void setPriceDocument2VoConverter(PriceDocument2VoConverter priceDocument2VoConverter) {
        this.priceDocument2VoConverter = priceDocument2VoConverter;
    }*/

    @Autowired
    public void setPaymentMethodDocument2VoConverter(PaymentMethodDocument2VoConverter paymentMethodDocument2VoConverter) {
        this.paymentMethodDocument2VoConverter = paymentMethodDocument2VoConverter;
    }

    /*@Autowired
    public void setItemDocument2VoConverter(ItemDocument2VoConverter itemDocument2VoConverter) {
        this.itemDocument2VoConverter = itemDocument2VoConverter;
    }*/

    public List<PaymentMethodVo> paymentMethodDocument2DetailedVo(List<? extends PaymentMethodDocument> paymentMethodDocumentList) {
        List<PaymentMethodVo> paymentMethodDetailsList = new LinkedList<>();
        if(paymentMethodDocumentList != null && !paymentMethodDocumentList.isEmpty()) {
            for(PaymentMethodDocument entity : paymentMethodDocumentList) {
                PaymentMethodVo vo = paymentMethodDocument2VoConverter.convert(entity);
                log.debug("Converting {} to {}", entity, vo);
                paymentMethodDetailsList.add(vo);
            }
        }
        return paymentMethodDetailsList;
    }

    /*public List<ItemVo> itemDocument2DetailedVo(List<? extends ItemDocument> itemDocumentList) {
        List<ItemVo> itemDetailsList = new LinkedList<>();
        if(itemDocumentList != null && !itemDocumentList.isEmpty()) {
            for(ItemDocument entity : itemDocumentList) {
                ItemVo vo = itemDocument2VoConverter.convert(entity);
                log.debug("Converting {} to {}", entity, vo);
                itemDetailsList.add(vo);
            }
        }
        return itemDetailsList;
    }
    
    public ItemVo itemDocument2DetailedVo(ItemDocument itemDocument) throws ItemException {
        if(itemDocument != null) {
            ItemVo vo = itemDocument2VoConverter.convert(itemDocument);
            log.debug("Converting {} to {}", itemDocument, vo);
            return vo;
        }
        throw new ItemException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[] { "item entity is null" });
    }
    
    */

    public PaymentMethodVo paymentMethodDocument2DetailedVo(PaymentMethodDocument paymentMethodDocument) throws PaymentMethodException {
        if(paymentMethodDocument != null) {
            PaymentMethodVo vo = paymentMethodDocument2VoConverter.convert(paymentMethodDocument);
            log.debug("Converting {} to {}", paymentMethodDocument, vo);
            return vo;
        }
        throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[] { "paymentMethod entity is null" });
    }

    /*public List<PriceVo> priceDocument2DetailedVo(List<PriceDocument> priceDocumentList) {
        List<PriceVo> priceDetailsList = new LinkedList<>();
        if(priceDocumentList != null && !priceDocumentList.isEmpty()) {
            for(PriceDocument entity : priceDocumentList) {
                PriceVo vo = priceDocument2VoConverter.convert(entity);
                log.debug("Converting {} to {}", entity, vo);
                priceDetailsList.add(vo);
            }
        }
        return priceDetailsList;
    }

    public PriceVo priceDocument2DetailedVo(PriceDocument priceDocument) throws PriceException {
        if(priceDocument != null) {
            PriceVo vo = priceDocument2VoConverter.convert(priceDocument);
            log.debug("Converting {} to {}", priceDocument, vo);
            return vo;
        }
        throw new PriceException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[] { "price entity is null" });
    }*/
}
