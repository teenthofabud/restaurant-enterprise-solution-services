package com.teenthofabud.restaurant.solution.cookbook.integration.inventory.proxy.impl;

import com.teenthofabud.core.common.data.vo.HealthVo;
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
    public HealthVo health(String status) {
        log.debug("Falling back to default implementation of querying health status: {}", status);
        return new HealthVo();
    }
}
