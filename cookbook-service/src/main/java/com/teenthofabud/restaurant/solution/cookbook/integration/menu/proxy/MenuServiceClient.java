package com.teenthofabud.restaurant.solution.cookbook.integration.menu.proxy;

import com.teenthofabud.core.common.data.vo.HealthVo;
import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.configuration.MenuServiceIntegrationConfiguration;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.data.ItemVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.error.MenuServiceClientExceptionHandler;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(value = MenuServiceClient.SERVICE_CLIENT_NAME, url = "${res.cookbook.menu.service.url}", configuration = MenuServiceIntegrationConfiguration.class)
public interface MenuServiceClient {

    public static final String SERVICE_CLIENT_NAME = "menu-service";

    @GetMapping("/item/{id}")
    @TOABFeignErrorHandler(MenuServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public ItemVo getItemDetailsById(@PathVariable(required = true) String id);

    @GetMapping("/actuator/health")
    @TOABFeignErrorHandler(MenuServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public HealthVo health(@RequestParam(required = false) String status);

}