package com.teenthofabud.restaurant.solution.menu.price.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
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
import org.springframework.validation.Validator;

import java.util.Currency;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@Component
@Slf4j
public class PriceFormValidator implements Validator {

    private List<String> fieldsToEscape;
    private MenuServiceHelper menuServiceHelper;
    private ItemService itemService;

    @Autowired
    public void setMenuServiceHelper(MenuServiceHelper menuServiceHelper) {
        this.menuServiceHelper = menuServiceHelper;
    }

    @Autowired
    public void setItemService(ItemService itemService) {
        this.itemService = itemService;
    }

    @Value("#{'${res.menu.price.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }
    
    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(PriceForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        PriceForm form = (PriceForm) target;
        if(!fieldsToEscape.contains("amount") && (form.getAmount() == null || form.getAmount() <= 0)) {
            log.debug("PriceForm.amount is empty");
            errors.rejectValue("amount", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("currencyId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCurrencyId()))) {
            log.debug("PriceForm.currencyId is empty");
            errors.rejectValue("currencyId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("currencyId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCurrencyId()))) {
            if(!menuServiceHelper.isCurrencyCodeValid(form.getCurrencyId())) {
                log.debug("PriceForm.currencyId is invalid");
                errors.rejectValue("currencyId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        if(!fieldsToEscape.contains("itemId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getItemId()))) {
            log.debug("PriceForm.itemId is empty");
            errors.rejectValue("itemId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("itemId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getItemId()))) {
            String itemId = form.getItemId();
            try {
                Long.parseLong(itemId);
            } catch (NumberFormatException e) {
                log.debug("PriceForm.itemId is invalid");
                errors.rejectValue("itemId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return;
            }
            try {
                ItemVo itemVo = itemService.retrieveDetailsById(itemId, Optional.of(TOABCascadeLevel.ONE));
                if(!itemVo.getActive()) {
                    log.debug("PriceForm.itemId is inactive");
                    errors.rejectValue("itemId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (ItemException e) {
                log.debug("PriceForm.itemId is invalid");
                errors.rejectValue("itemId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return;
            }
        }
    }

}
