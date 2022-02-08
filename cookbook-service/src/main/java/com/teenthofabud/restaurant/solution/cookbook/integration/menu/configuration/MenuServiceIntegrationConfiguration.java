package com.teenthofabud.restaurant.solution.cookbook.integration.menu.configuration;

import com.teenthofabud.restaurant.solution.cookbook.integration.menu.proxy.MenuServiceClient;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.proxy.impl.MenuServiceClientFallbackImpl;
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
public class MenuServiceIntegrationConfiguration {

    private CircuitBreakerRegistry circuitBreakerRegistry;
    private MenuServiceClientFallbackImpl menuServiceClientFallback;

    @Autowired
    @Qualifier("menuServiceClientFallback")
    public void setMenuServiceClientFallback(MenuServiceClientFallbackImpl menuServiceClientFallback) {
        this.menuServiceClientFallback = menuServiceClientFallback;
    }

    @Autowired
    public void setCircuitBreakerRegistry(CircuitBreakerRegistry circuitBreakerRegistry) {
        this.circuitBreakerRegistry = circuitBreakerRegistry;
    }

    @Bean
    @Scope(scopeName = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public Feign.Builder itemServiceClientFeignBuilder() {
        CircuitBreaker circuitBreaker = circuitBreakerRegistry.circuitBreaker(MenuServiceClient.SERVICE_CLIENT_NAME);
        FeignDecorators decorators = FeignDecorators.builder()
                .withCircuitBreaker(circuitBreaker)
                .withFallback(menuServiceClientFallback, CallNotPermittedException.class)
                .build();
        return Resilience4jFeign.builder(decorators);
    }

}
