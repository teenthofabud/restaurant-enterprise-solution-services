package com.teenthofabud.restaurant.solution.cookbook.integration.inventory.configuration;

import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.proxy.InventoryServiceClient;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.proxy.impl.InventoryServiceClientFallbackImpl;
import feign.Feign;
import io.github.resilience4j.circuitbreaker.CallNotPermittedException;
import io.github.resilience4j.circuitbreaker.CircuitBreaker;
import io.github.resilience4j.circuitbreaker.CircuitBreakerRegistry;
import io.github.resilience4j.feign.FeignDecorators;
import io.github.resilience4j.feign.Resilience4jFeign;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

@Configuration
public class InventoryServiceIntegrationConfiguration {

    private CircuitBreakerRegistry circuitBreakerRegistry;
    private InventoryServiceClientFallbackImpl inventoryServiceClientFallback;

    @Autowired
    @Qualifier("inventoryServiceClientFallback")
    public void setInventoryServiceClientFallback(InventoryServiceClientFallbackImpl inventoryServiceClientFallback) {
        this.inventoryServiceClientFallback = inventoryServiceClientFallback;
    }

    @Autowired
    public void setCircuitBreakerRegistry(CircuitBreakerRegistry circuitBreakerRegistry) {
        this.circuitBreakerRegistry = circuitBreakerRegistry;
    }

    @Bean
    @Scope(scopeName = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public Feign.Builder productServiceClientFeignBuilder() {
        CircuitBreaker circuitBreaker = circuitBreakerRegistry.circuitBreaker(InventoryServiceClient.SERVICE_CLIENT_NAME);
        FeignDecorators decorators = FeignDecorators.builder()
                .withCircuitBreaker(circuitBreaker)
                .withFallback(inventoryServiceClientFallback, CallNotPermittedException.class)
                .build();
        return Resilience4jFeign.builder(decorators);
    }

}
