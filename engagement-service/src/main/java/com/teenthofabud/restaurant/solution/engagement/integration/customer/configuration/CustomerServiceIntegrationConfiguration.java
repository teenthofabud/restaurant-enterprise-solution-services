package com.teenthofabud.restaurant.solution.engagement.integration.customer.configuration;

import com.teenthofabud.restaurant.solution.engagement.integration.customer.proxy.CustomerServiceClient;
import com.teenthofabud.restaurant.solution.engagement.integration.customer.proxy.impl.CustomerServiceClientFallbackImpl;
import feign.Feign;
import io.github.resilience4j.circuitbreaker.CallNotPermittedException;
import io.github.resilience4j.circuitbreaker.CircuitBreaker;
import io.github.resilience4j.circuitbreaker.CircuitBreakerRegistry;
import io.github.resilience4j.feign.FeignDecorators;
import io.github.resilience4j.feign.Resilience4jFeign;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.cloud.loadbalancer.annotation.LoadBalancerClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

@Configuration
public class CustomerServiceIntegrationConfiguration {

    private CircuitBreakerRegistry circuitBreakerRegistry;
    private CustomerServiceClientFallbackImpl accountServiceClientFallback;

    @Autowired
    @Qualifier("customerServiceClientFallback")
    public void setAccountServiceClientFallback(CustomerServiceClientFallbackImpl accountServiceClientFallback) {
        this.accountServiceClientFallback = accountServiceClientFallback;
    }

    @Autowired
    public void setCircuitBreakerRegistry(CircuitBreakerRegistry circuitBreakerRegistry) {
        this.circuitBreakerRegistry = circuitBreakerRegistry;
    }

    @Bean
    @Scope(scopeName = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public Feign.Builder accountServiceClientFeignBuilder() {
        CircuitBreaker circuitBreaker = circuitBreakerRegistry.circuitBreaker(CustomerServiceClient.SERVICE_CLIENT_NAME);
        FeignDecorators decorators = FeignDecorators.builder()
                .withCircuitBreaker(circuitBreaker)
                .withFallback(accountServiceClientFallback, CallNotPermittedException.class)
                .build();
        return Resilience4jFeign.builder(decorators);
    }

}
