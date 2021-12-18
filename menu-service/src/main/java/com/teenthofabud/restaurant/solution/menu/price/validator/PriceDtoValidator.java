package com.teenthofabud.restaurant.solution.menu.price.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemException;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemVo;
import com.teenthofabud.restaurant.solution.menu.item.service.ItemService;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceDto;
import com.teenthofabud.restaurant.solution.menu.utils.MenuServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class PriceDtoValidator implements Validator {

    private List<String> fieldsToEscape;
    private ItemService itemService;
    private MenuServiceHelper menuServiceHelper;

    @Value("#{'${res.menu.item.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setItemService(ItemService itemService) {
        this.itemService = itemService;
    }

    @Autowired
    public void setMenuServiceHelper(MenuServiceHelper menuServiceHelper) {
        this.menuServiceHelper = menuServiceHelper;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(PriceDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        PriceDto dto = (PriceDto) target;
        Optional<String> optAmount = dto.getAmount();
        if(!fieldsToEscape.contains("amount") && optAmount.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optAmount.get()))) {
            errors.rejectValue("amount", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("PriceDto.amount is invalid");
            return;
        } if(!fieldsToEscape.contains("amount") && optAmount.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optAmount.get()))) {
            try {
                Double price = Double.parseDouble(optAmount.get());
                if(price <= 0.0d) {
                    errors.rejectValue("amount", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                    log.debug("PriceDto.amount is invalid");
                    return;
                }
            } catch (NumberFormatException e) {
                errors.rejectValue("amount", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                log.debug("PriceDto.amount is invalid");
                return;
            }
        }

        Optional<String> optItemId = dto.getItemId();
        if(!fieldsToEscape.contains("itemId") && optItemId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optItemId.get()))) {
            errors.rejectValue("itemId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("PriceDto.itemId is invalid");
            return;
        } else if(!fieldsToEscape.contains("itemId") && optItemId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optItemId.get()))) {
            String itemId = optItemId.get();
            try {
                Long.parseLong(itemId);
            } catch (NumberFormatException e) {
                log.debug("PriceDto.itemId is invalid");
                errors.rejectValue("itemId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return;
            }
            try {
                ItemVo itemVo = itemService.retrieveDetailsById(itemId, Optional.of(TOABCascadeLevel.ONE));
                if(!itemVo.getActive()) {
                    log.debug("PriceDto.itemId is inactive");
                    errors.rejectValue("itemId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (ItemException e) {
                log.debug("PriceDto.itemId is invalid");
                errors.rejectValue("itemId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                log.debug("PriceDto.active is invalid");
                return;
            }
        }

        Optional<String> optCurrencyId = dto.getCurrencyId();
        if(!fieldsToEscape.contains("currencyId") && optCurrencyId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optCurrencyId.get()))) {
            errors.rejectValue("currencyId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
            log.debug("PriceDto.currencyId is invalid");
            return;
        } else if(!fieldsToEscape.contains("currencyId") && optCurrencyId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optCurrencyId.get()))) {
            if(!menuServiceHelper.isCurrencyCodeValid(optCurrencyId.get())) {
                log.debug("PriceDto.currencyId is invalid");
                errors.rejectValue("currencyId", MenuErrorCode.MENU_ATTRIBUTE_INVALID.name());
                return;
            }
        }
    }

}
