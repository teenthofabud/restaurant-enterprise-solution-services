package com.teenthofabud.restaurant.solution.customer.integration.metadata.configuration;

import com.teenthofabud.restaurant.solution.customer.integration.metadata.proxy.MetadataServiceClient;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.proxy.impl.MetadataServiceClientFallbackImpl;
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
public class MetadataServiceIntegrationConfiguration {

    private CircuitBreakerRegistry circuitBreakerRegistry;
    private MetadataServiceClientFallbackImpl genderServiceClientFallback;

    @Autowired
    @Qualifier("genderServiceClientFallback")
    public void setGenderServiceClientFallback(MetadataServiceClientFallbackImpl genderServiceClientFallback) {
        this.genderServiceClientFallback = genderServiceClientFallback;
    }

    @Autowired
    public void setCircuitBreakerRegistry(CircuitBreakerRegistry circuitBreakerRegistry) {
        this.circuitBreakerRegistry = circuitBreakerRegistry;
    }

    @Bean
    @Scope(scopeName = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public Feign.Builder genderServiceClientFeignBuilder() {
        CircuitBreaker circuitBreaker = circuitBreakerRegistry.circuitBreaker(MetadataServiceClient.SERVICE_CLIENT_NAME);
        FeignDecorators decorators = FeignDecorators.builder()
                .withCircuitBreaker(circuitBreaker)
                .withFallback(genderServiceClientFallback, CallNotPermittedException.class)
                .build();
        return Resilience4jFeign.builder(decorators);
    }

}
