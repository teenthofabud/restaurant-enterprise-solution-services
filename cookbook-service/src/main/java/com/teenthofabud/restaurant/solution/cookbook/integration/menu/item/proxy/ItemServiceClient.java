package com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.proxy;

import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.configuration.MenuServiceItemIntegrationConfiguration;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.data.ItemVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.error.ItemServiceClientExceptionHandler;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = ItemServiceClient.SERVICE_CLIENT_NAME, url = "${cookbook.menu.item-service.url}", configuration = MenuServiceItemIntegrationConfiguration.class)
public interface ItemServiceClient {

    public static final String SERVICE_CLIENT_NAME = "menu-service-item-client";

    @GetMapping("/{id}")
    @TOABFeignErrorHandler(ItemServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public ItemVo getItemDetailsById(@PathVariable(required = true) String id);

}