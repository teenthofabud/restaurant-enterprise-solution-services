package com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.configuration;

import com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.proxy.TableServiceClient;
import com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.proxy.impl.TableServiceClientFallbackImpl;
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
public class EstablishmentAreaServiceTableIntegrationConfiguration {

    private CircuitBreakerRegistry circuitBreakerRegistry;
    private TableServiceClientFallbackImpl tableServiceClientFallback;

    @Autowired
    @Qualifier("tableServiceClientFallback")
    public void setTableServiceClientFallback(TableServiceClientFallbackImpl tableServiceClientFallback) {
        this.tableServiceClientFallback = tableServiceClientFallback;
    }

    @Autowired
    public void setCircuitBreakerRegistry(CircuitBreakerRegistry circuitBreakerRegistry) {
        this.circuitBreakerRegistry = circuitBreakerRegistry;
    }

    @Bean
    @Scope(scopeName = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public Feign.Builder tableServiceClientFeignBuilder() {
        CircuitBreaker circuitBreaker = circuitBreakerRegistry.circuitBreaker(TableServiceClient.SERVICE_CLIENT_NAME);
        FeignDecorators decorators = FeignDecorators.builder()
                .withCircuitBreaker(circuitBreaker)
                .withFallback(tableServiceClientFallback, CallNotPermittedException.class)
                .build();
        return Resilience4jFeign.builder(decorators);
    }

}
