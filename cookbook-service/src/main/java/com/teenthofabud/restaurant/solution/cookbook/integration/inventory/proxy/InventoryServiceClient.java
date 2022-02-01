package com.teenthofabud.restaurant.solution.cookbook.integration.inventory.proxy;

import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.configuration.InventoryServiceIntegrationConfiguration;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.data.HealthVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.data.ProductVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.error.InventoryServiceClientExceptionHandler;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = InventoryServiceClient.SERVICE_CLIENT_NAME, url = "${cookbook.inventory.service.url}", configuration = InventoryServiceIntegrationConfiguration.class)
public interface InventoryServiceClient {

    public static final String SERVICE_CLIENT_NAME = "inventory-service";

    @GetMapping("/product/{id}")
    @TOABFeignErrorHandler(InventoryServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public ProductVo getProductDetailsById(@PathVariable(required = true) String id);

    @GetMapping("/actuator/health")
    @TOABFeignErrorHandler(InventoryServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public HealthVo health();

}