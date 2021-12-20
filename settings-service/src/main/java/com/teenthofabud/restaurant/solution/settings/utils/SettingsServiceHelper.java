package com.teenthofabud.restaurant.solution.settings.utils;

import com.teenthofabud.restaurant.solution.settings.charge.converter.ChargeDocument2VoConverter;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeDocument;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeException;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeVo;
import com.teenthofabud.restaurant.solution.settings.discount.converter.DiscountDocument2VoConverter;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountDocument;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountException;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountVo;
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
    private ChargeDocument2VoConverter chargeDocument2VoConverter;
    private DiscountDocument2VoConverter discountDocument2VoConverter;

    @Autowired
    public void setDiscountDocument2VoConverter(DiscountDocument2VoConverter discountDocument2VoConverter) {
        this.discountDocument2VoConverter = discountDocument2VoConverter;
    }

    @Autowired
    public void setPaymentMethodDocument2VoConverter(PaymentMethodDocument2VoConverter paymentMethodDocument2VoConverter) {
        this.paymentMethodDocument2VoConverter = paymentMethodDocument2VoConverter;
    }

    @Autowired
    public void setChargeDocument2VoConverter(ChargeDocument2VoConverter chargeDocument2VoConverter) {
        this.chargeDocument2VoConverter = chargeDocument2VoConverter;
    }

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

    public List<ChargeVo> chargeDocument2DetailedVo(List<? extends ChargeDocument> chargeDocumentList) {
        List<ChargeVo> chargeDetailsList = new LinkedList<>();
        if(chargeDocumentList != null && !chargeDocumentList.isEmpty()) {
            for(ChargeDocument entity : chargeDocumentList) {
                ChargeVo vo = chargeDocument2VoConverter.convert(entity);
                log.debug("Converting {} to {}", entity, vo);
                chargeDetailsList.add(vo);
            }
        }
        return chargeDetailsList;
    }
    
    public ChargeVo chargeDocument2DetailedVo(ChargeDocument chargeDocument) throws ChargeException {
        if(chargeDocument != null) {
            ChargeVo vo = chargeDocument2VoConverter.convert(chargeDocument);
            log.debug("Converting {} to {}", chargeDocument, vo);
            return vo;
        }
        throw new ChargeException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[] { "charge entity is null" });
    }

    public PaymentMethodVo paymentMethodDocument2DetailedVo(PaymentMethodDocument paymentMethodDocument) throws PaymentMethodException {
        if(paymentMethodDocument != null) {
            PaymentMethodVo vo = paymentMethodDocument2VoConverter.convert(paymentMethodDocument);
            log.debug("Converting {} to {}", paymentMethodDocument, vo);
            return vo;
        }
        throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[] { "paymentMethod entity is null" });
    }

    public List<DiscountVo> discountDocument2DetailedVo(List<DiscountDocument> discountDocumentList) {
        List<DiscountVo> discountDetailsList = new LinkedList<>();
        if(discountDocumentList != null && !discountDocumentList.isEmpty()) {
            for(DiscountDocument entity : discountDocumentList) {
                DiscountVo vo = discountDocument2VoConverter.convert(entity);
                log.debug("Converting {} to {}", entity, vo);
                discountDetailsList.add(vo);
            }
        }
        return discountDetailsList;
    }

    public DiscountVo discountDocument2DetailedVo(DiscountDocument discountDocument) throws DiscountException {
        if(discountDocument != null) {
            DiscountVo vo = discountDocument2VoConverter.convert(discountDocument);
            log.debug("Converting {} to {}", discountDocument, vo);
            return vo;
        }
        throw new DiscountException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[] { "discount entity is null" });
    }
}
