package com.teenthofabud.restaurant.solution.cookbook.integration.menu.proxy.impl;

import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.data.HealthVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.data.ItemVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.proxy.MenuServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component("menuServiceClientFallback")
@Slf4j
public class MenuServiceClientFallbackImpl implements MenuServiceClient {

    @Override
    public ItemVo getItemDetailsById(String id) {
        log.debug("Falling back to default implementation of getting item details by id");
        return new ItemVo();
    }

    @Override
    public HealthVo health() {
        log.debug("Falling back to default implementation of querying halth status");
        return new HealthVo();
    }

}
