package com.teenthofabud.restaurant.solution.settings.utils;

import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.settings.charge.converter.ChargeDocument2VoConverter;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeDocument;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeVo;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.converter.DeliveryPartnerDocument2VoConverter;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerDocument;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerVo;
import com.teenthofabud.restaurant.solution.settings.device.converter.DeviceDocument2VoConverter;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceDocument;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceVo;
import com.teenthofabud.restaurant.solution.settings.discount.converter.DiscountDocument2VoConverter;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountDocument;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountVo;
import com.teenthofabud.restaurant.solution.settings.internationalization.converter.LanguageDocument2VoConverter;
import com.teenthofabud.restaurant.solution.settings.internationalization.data.LanguageDocument;
import com.teenthofabud.restaurant.solution.settings.internationalization.data.LanguageVo;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.converter.PaymentMethodDocument2VoConverter;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodDocument;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodVo;
import com.teenthofabud.restaurant.solution.settings.template.converter.TemplateDocument2VoConverter;
import com.teenthofabud.restaurant.solution.settings.template.data.TemplateDocument;
import com.teenthofabud.restaurant.solution.settings.template.data.TemplateVo;
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
    private DeliveryPartnerDocument2VoConverter deliveryPartnerDocument2VoConverter;
    private TemplateDocument2VoConverter templateDocument2VoConverter;
    private DeviceDocument2VoConverter deviceDocument2VoConverter;
    private LanguageDocument2VoConverter languageDocument2VoConverter;

    @Autowired
    public void setDeviceDocument2VoConverter(DeviceDocument2VoConverter deviceDocument2VoConverter) {
        this.deviceDocument2VoConverter = deviceDocument2VoConverter;
    }

    @Autowired
    public void setTemplateDocument2VoConverter(TemplateDocument2VoConverter templateDocument2VoConverter) {
        this.templateDocument2VoConverter = templateDocument2VoConverter;
    }

    @Autowired
    public void setDeliveryPartnerDocument2VoConverter(DeliveryPartnerDocument2VoConverter deliveryPartnerDocument2VoConverter) {
        this.deliveryPartnerDocument2VoConverter = deliveryPartnerDocument2VoConverter;
    }

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

    @Autowired
    public void setLanguageDocument2VoConverter(LanguageDocument2VoConverter languageDocument2VoConverter){
        this.languageDocument2VoConverter = languageDocument2VoConverter;
    }

    public List<DeliveryPartnerVo> deliveryPartnerDocument2DetailedVo(List<? extends DeliveryPartnerDocument> deliveryPartnerDocumentList) throws TOABSystemException {
        List<DeliveryPartnerVo> deliveryPartnerDetailsList = new LinkedList<>();
        if(deliveryPartnerDocumentList != null && !deliveryPartnerDocumentList.isEmpty()) {
            for(DeliveryPartnerDocument document : deliveryPartnerDocumentList) {
                DeliveryPartnerVo vo = this.deliveryPartnerDocument2DetailedVo(document);
                deliveryPartnerDetailsList.add(vo);
            }
        }
        return deliveryPartnerDetailsList;
    }

    public DeliveryPartnerVo deliveryPartnerDocument2DetailedVo(DeliveryPartnerDocument deliveryPartnerDocument) throws TOABSystemException {
        if(deliveryPartnerDocument != null) {
            DeliveryPartnerVo vo = deliveryPartnerDocument2VoConverter.convert(deliveryPartnerDocument);
            log.debug("Converting {} to {}", deliveryPartnerDocument, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "deliveryPartner document is null" });
    }

    public List<PaymentMethodVo> paymentMethodDocument2DetailedVo(List<? extends PaymentMethodDocument> paymentMethodDocumentList) throws TOABSystemException {
        List<PaymentMethodVo> paymentMethodDetailsList = new LinkedList<>();
        if(paymentMethodDocumentList != null && !paymentMethodDocumentList.isEmpty()) {
            for(PaymentMethodDocument document : paymentMethodDocumentList) {
                PaymentMethodVo vo = this.paymentMethodDocument2DetailedVo(document);
                paymentMethodDetailsList.add(vo);
            }
        }
        return paymentMethodDetailsList;
    }

    public PaymentMethodVo paymentMethodDocument2DetailedVo(PaymentMethodDocument paymentMethodDocument) throws TOABSystemException {
        if(paymentMethodDocument != null) {
            PaymentMethodVo vo = paymentMethodDocument2VoConverter.convert(paymentMethodDocument);
            log.debug("Converting {} to {}", paymentMethodDocument, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "paymentMethod document is null" });
    }

    public List<ChargeVo> chargeDocument2DetailedVo(List<? extends ChargeDocument> chargeDocumentList) throws TOABSystemException {
        List<ChargeVo> chargeDetailsList = new LinkedList<>();
        if(chargeDocumentList != null && !chargeDocumentList.isEmpty()) {
            for(ChargeDocument document : chargeDocumentList) {
                ChargeVo vo = this.chargeDocument2DetailedVo(document);
                chargeDetailsList.add(vo);
            }
        }
        return chargeDetailsList;
    }
    
    public ChargeVo chargeDocument2DetailedVo(ChargeDocument chargeDocument) throws TOABSystemException {
        if(chargeDocument != null) {
            ChargeVo vo = chargeDocument2VoConverter.convert(chargeDocument);
            log.debug("Converting {} to {}", chargeDocument, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "charge document is null" });
    }

    public List<DiscountVo> discountDocument2DetailedVo(List<DiscountDocument> discountDocumentList) throws TOABSystemException {
        List<DiscountVo> discountDetailsList = new LinkedList<>();
        if(discountDocumentList != null && !discountDocumentList.isEmpty()) {
            for(DiscountDocument document : discountDocumentList) {
                DiscountVo vo = this.discountDocument2DetailedVo(document);
                discountDetailsList.add(vo);
            }
        }
        return discountDetailsList;
    }

    public DiscountVo discountDocument2DetailedVo(DiscountDocument discountDocument) throws TOABSystemException {
        if(discountDocument != null) {
            DiscountVo vo = discountDocument2VoConverter.convert(discountDocument);
            log.debug("Converting {} to {}", discountDocument, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "discount document is null" });
    }

    public List<TemplateVo> templateDocument2DetailedVo(List<? extends TemplateDocument> templateDocumentList) {
        List<TemplateVo> templateDetailsList = new LinkedList<>();
        if(templateDocumentList != null && !templateDocumentList.isEmpty()) {
            for(TemplateDocument document : templateDocumentList) {
                TemplateVo vo = this.templateDocument2DetailedVo(document);
                templateDetailsList.add(vo);
            }
        }
        return templateDetailsList;
    }

    public TemplateVo templateDocument2DetailedVo(TemplateDocument templateDocument) {
        if(templateDocument != null) {
            TemplateVo vo = templateDocument2VoConverter.convert(templateDocument);
            log.debug("Converting {} to {}", templateDocument, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "template document is null" });
    }

    public List<DeviceVo> deviceDocument2DetailedVo(List<? extends DeviceDocument> deviceDocumentList) {
        List<DeviceVo> deviceDetailsList = new LinkedList<>();
        if(deviceDocumentList != null && !deviceDocumentList.isEmpty()) {
            for(DeviceDocument document : deviceDocumentList) {
                DeviceVo vo = this.deviceDocument2DetailedVo(document);
                deviceDetailsList.add(vo);
            }
        }
        return deviceDetailsList;
    }

    public DeviceVo deviceDocument2DetailedVo(DeviceDocument deviceDocument) {
        if(deviceDocument != null) {
            DeviceVo vo = deviceDocument2VoConverter.convert(deviceDocument);
            log.debug("Converting {} to {}", deviceDocument, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "device document is null" });
    }

    public List<LanguageVo> languageDocument2DetailedVo(List<? extends LanguageDocument> languageDocumentList){
        List<LanguageVo> languageDetailsList = new LinkedList<>();
        if(languageDocumentList != null && !languageDocumentList.isEmpty()){
            for(LanguageDocument document : languageDocumentList){
                LanguageVo vo = this.languageDocument2DetailedVo(document);
                languageDetailsList.add(vo);
            }
        }
        return languageDetailsList;
    }

    public LanguageVo languageDocument2DetailedVo(LanguageDocument languageDocument) {
        if(languageDocument != null){
            LanguageVo vo = languageDocument2VoConverter.convert(languageDocument);
            log.debug("Converting {} to {}", languageDocument, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "language document is null" });
    }

}
