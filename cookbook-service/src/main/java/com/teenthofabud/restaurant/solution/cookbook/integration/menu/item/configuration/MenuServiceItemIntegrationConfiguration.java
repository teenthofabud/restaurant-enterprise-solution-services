package com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.configuration;

import com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.proxy.ItemServiceClient;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.proxy.impl.ItemServiceClientFallbackImpl;
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
public class MenuServiceItemIntegrationConfiguration {

    private CircuitBreakerRegistry circuitBreakerRegistry;
    private ItemServiceClientFallbackImpl itemServiceClientFallback;

    @Autowired
    @Qualifier("itemServiceClientFallback")
    public void setItemServiceClientFallback(ItemServiceClientFallbackImpl itemServiceClientFallback) {
        this.itemServiceClientFallback = itemServiceClientFallback;
    }

    @Autowired
    public void setCircuitBreakerRegistry(CircuitBreakerRegistry circuitBreakerRegistry) {
        this.circuitBreakerRegistry = circuitBreakerRegistry;
    }

    @Bean
    @Scope(scopeName = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public Feign.Builder itemServiceClientFeignBuilder() {
        CircuitBreaker circuitBreaker = circuitBreakerRegistry.circuitBreaker(ItemServiceClient.SERVICE_CLIENT_NAME);
        FeignDecorators decorators = FeignDecorators.builder()
                .withCircuitBreaker(circuitBreaker)
                .withFallback(itemServiceClientFallback, CallNotPermittedException.class)
                .build();
        return Resilience4jFeign.builder(decorators);
    }

}
