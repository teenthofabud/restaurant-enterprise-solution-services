package com.teenthofabud.restaurant.solution.menu.price.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemException;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemVo;
import com.teenthofabud.restaurant.solution.menu.item.service.ItemService;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceForm;
import com.teenthofabud.restaurant.solution.menu.utils.MenuServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class PriceFormRelaxedValidator implements RelaxedValidator<PriceForm>  {

    private List<String> fieldsToEscape;
    private ItemService itemService;
    private MenuServiceHelper menuServiceHelper;

    @Value("#{'${res.menu.item.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setMenuServiceHelper(MenuServiceHelper menuServiceHelper) {
        this.menuServiceHelper = menuServiceHelper;
    }
    
    @Autowired
    public void setItemService(ItemService itemService) {
        this.itemService = itemService;
    }

    @Override
    public Boolean validateLoosely(PriceForm form, Errors errors) {
        if(!fieldsToEscape.contains("amount") && form.getAmount() != null && form.getAmount() <= 0) {
            errors.rejectValue("amount", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("PriceForm.amount is empty");
            return false;
        }
        log.debug("PriceForm.amount is valid");

        if(!fieldsToEscape.contains("currencyId") && form.getCurrencyId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCurrencyId()))) {
            errors.rejectValue("currencyId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("PriceForm.currencyId is empty");
            return false;
        } else if(!fieldsToEscape.contains("currencyId") && form.getCurrencyId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getCurrencyId()))) {
            if(!menuServiceHelper.isCurrencyCodeValid(form.getCurrencyId())) {
                log.debug("PriceForm.currencyId is invalid");
                errors.rejectValue("currencyId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("PriceForm.currencyId is valid");

        if(!fieldsToEscape.contains("itemId") && form.getItemId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getItemId()))) {
            log.debug("PriceForm.itemId is empty");
            errors.rejectValue("itemId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("itemId") && form.getItemId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getItemId()))) {
            String itemId = form.getItemId();
            try {
                Long.parseLong(itemId);
            } catch (NumberFormatException e) {
                log.debug("PriceForm.itemId is invalid");
                errors.rejectValue("itemId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return false;
            }
            try {
                ItemVo itemVo = itemService.retrieveDetailsById(itemId, Optional.of(TOABCascadeLevel.ONE));
                if(!itemVo.getActive()) {
                    log.debug("PriceForm.itemId is inactive");
                    errors.rejectValue("itemId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                    return false;
                }
            } catch (ItemException e) {
                log.debug("PriceForm.itemId is invalid");
                errors.rejectValue("itemId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        return true;
    }
}
