package com.teenthofabud.restaurant.solution.cookbook.integration.menu.proxy;

import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.data.HealthVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.configuration.MenuServiceIntegrationConfiguration;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.data.ItemVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.error.MenuServiceClientExceptionHandler;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = MenuServiceClient.SERVICE_CLIENT_NAME, url = "${cookbook.menu.service.url}", configuration = MenuServiceIntegrationConfiguration.class)
public interface MenuServiceClient {

    public static final String SERVICE_CLIENT_NAME = "menu-service";

    @GetMapping("/menu/item/{id}")
    @TOABFeignErrorHandler(MenuServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public ItemVo getItemDetailsById(@PathVariable(required = true) String id);

    @GetMapping("/actuator/health")
    @TOABFeignErrorHandler(MenuServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public HealthVo health();

}