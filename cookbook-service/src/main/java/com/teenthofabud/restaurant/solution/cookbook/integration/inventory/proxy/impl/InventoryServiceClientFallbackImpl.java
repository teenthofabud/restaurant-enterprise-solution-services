package com.teenthofabud.restaurant.solution.cookbook.integration.inventory.proxy.impl;

import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.data.HealthVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.data.ProductVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.proxy.InventoryServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component("inventoryServiceClientFallback")
@Slf4j
public class InventoryServiceClientFallbackImpl implements InventoryServiceClient {

    @Override
    public ProductVo getProductDetailsById(String id) {
        log.debug("Falling back to default implementation of getting product details by id");
        return new ProductVo();
    }

    @Override
    public HealthVo health() {
        log.debug("Falling back to default implementation of querying halth status");
        return new HealthVo();
    }
}
