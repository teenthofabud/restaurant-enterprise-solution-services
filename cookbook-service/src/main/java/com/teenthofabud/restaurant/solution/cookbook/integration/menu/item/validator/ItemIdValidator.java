package com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.validator;

import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.data.ItemVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.proxy.ItemServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class ItemIdValidator implements Validator {

    private ItemServiceClient itemServiceClient;

    @Autowired
    public void setItemServiceClient(ItemServiceClient itemServiceClient) {
        this.itemServiceClient = itemServiceClient;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(String.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        String objectName = errors.getObjectName();
        String itemId = (String) target;
        log.debug("Validating itemId: {}", itemId);
        ItemVo itemVo = null;
        log.info("Requesting details of item with id: {}", itemId);
        itemVo = itemServiceClient.getItemDetailsById(itemId);
        log.info("Retrieved item: {} by id", itemVo);
        if(itemVo == null) {
            errors.reject(CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".itemId is invalid");
            return;
        }
        boolean emptyFileName = !StringUtils.hasText(StringUtils.trimWhitespace(itemVo.getName()));
        boolean emptyId = !StringUtils.hasText(StringUtils.trimWhitespace(itemVo.getId()));
        if(emptyFileName) {
            log.debug(objectName + ".item.name is invalid");
            errors.reject(CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return;
        }
        if(emptyId) {
            log.debug(objectName + ".item.itemId is invalid");
            errors.reject(CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!itemVo.getActive()) {
            errors.reject(CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".itemId is not active");
            return;
        }
    }

}
