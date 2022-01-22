package com.teenthofabud.restaurant.solution.session.integration.customer.account.configuration;

import com.teenthofabud.restaurant.solution.session.integration.customer.account.proxy.AccountServiceClient;
import com.teenthofabud.restaurant.solution.session.integration.customer.account.proxy.impl.AccountServiceClientFallbackImpl;
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
public class CustomerServiceAccountIntegrationConfiguration {

    private CircuitBreakerRegistry circuitBreakerRegistry;
    private AccountServiceClientFallbackImpl accountServiceClientFallback;

    @Autowired
    @Qualifier("accountServiceClientFallback")
    public void setAccountServiceClientFallback(AccountServiceClientFallbackImpl accountServiceClientFallback) {
        this.accountServiceClientFallback = accountServiceClientFallback;
    }

    @Autowired
    public void setCircuitBreakerRegistry(CircuitBreakerRegistry circuitBreakerRegistry) {
        this.circuitBreakerRegistry = circuitBreakerRegistry;
    }

    @Bean
    @Scope(scopeName = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public Feign.Builder accountServiceClientFeignBuilder() {
        CircuitBreaker circuitBreaker = circuitBreakerRegistry.circuitBreaker(AccountServiceClient.SERVICE_CLIENT_NAME);
        FeignDecorators decorators = FeignDecorators.builder()
                .withCircuitBreaker(circuitBreaker)
                .withFallback(accountServiceClientFallback, CallNotPermittedException.class)
                .build();
        return Resilience4jFeign.builder(decorators);
    }

}
