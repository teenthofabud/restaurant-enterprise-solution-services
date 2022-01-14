package com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.proxy.impl;

import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.data.ProductVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.proxy.ProductServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component("productServiceClientFallback")
@Slf4j
public class ProductServiceClientFallbackImpl implements ProductServiceClient {

    @Override
    public ProductVo getProductDetailsById(String id) {
        log.debug("Falling back to default implementation of getting product details by id");
        return new ProductVo();
    }
}
