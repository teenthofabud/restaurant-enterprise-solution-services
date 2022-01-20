package com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.proxy.impl;

import com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.data.ItemVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.proxy.ItemServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component("itemServiceClientFallback")
@Slf4j
public class ItemServiceClientFallbackImpl implements ItemServiceClient {

    @Override
    public ItemVo getItemDetailsById(String id) {
        log.debug("Falling back to default implementation of getting item details by id");
        return new ItemVo();
    }
}
