package com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.proxy;

import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.configuration.InventoryServiceProductIntegrationConfiguration;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.data.ProductVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.error.ProductServiceClientExceptionHandler;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = ProductServiceClient.SERVICE_CLIENT_NAME, url = "${cookbook.inventory.product-service.url}", configuration = InventoryServiceProductIntegrationConfiguration.class)
public interface ProductServiceClient {

    public static final String SERVICE_CLIENT_NAME = "inventory-service-product-client";

    @GetMapping("/{id}")
    @TOABFeignErrorHandler(ProductServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public ProductVo getProductDetailsById(@PathVariable(required = true) String id);

}