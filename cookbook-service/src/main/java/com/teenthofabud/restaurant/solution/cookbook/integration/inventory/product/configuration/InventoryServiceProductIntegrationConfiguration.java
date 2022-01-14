package com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.configuration;

import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.proxy.ProductServiceClient;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.proxy.impl.ProductServiceClientFallbackImpl;
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
public class InventoryServiceProductIntegrationConfiguration {

    private CircuitBreakerRegistry circuitBreakerRegistry;
    private ProductServiceClientFallbackImpl productServiceClientFallback;

    @Autowired
    @Qualifier("productServiceClientFallback")
    public void setProductServiceClientFallback(ProductServiceClientFallbackImpl productServiceClientFallback) {
        this.productServiceClientFallback = productServiceClientFallback;
    }

    @Autowired
    public void setCircuitBreakerRegistry(CircuitBreakerRegistry circuitBreakerRegistry) {
        this.circuitBreakerRegistry = circuitBreakerRegistry;
    }

    @Bean
    @Scope(scopeName = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public Feign.Builder productServiceClientFeignBuilder() {
        CircuitBreaker circuitBreaker = circuitBreakerRegistry.circuitBreaker(ProductServiceClient.SERVICE_CLIENT_NAME);
        FeignDecorators decorators = FeignDecorators.builder()
                .withCircuitBreaker(circuitBreaker)
                .withFallback(productServiceClientFallback, CallNotPermittedException.class)
                .build();
        return Resilience4jFeign.builder(decorators);
    }

}
