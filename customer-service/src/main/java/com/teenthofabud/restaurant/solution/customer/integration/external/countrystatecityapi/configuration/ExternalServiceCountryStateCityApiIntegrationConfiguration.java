package com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.configuration;

import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.proxy.CountryStateCityApiClient;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.proxy.impl.CountryStateCityApiClientFallbackImpl;
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
public class ExternalServiceCountryStateCityApiIntegrationConfiguration {

    private CircuitBreakerRegistry circuitBreakerRegistry;
    private CountryStateCityApiClientFallbackImpl countryStateCityApiClientFallback;

    @Autowired
    @Qualifier("countryStateCityApiClientFallback")
    public void setCountryStateCityApiClientFallback(CountryStateCityApiClientFallbackImpl countryStateCityApiClientFallback) {
        this.countryStateCityApiClientFallback = countryStateCityApiClientFallback;
    }

    @Autowired
    public void setCircuitBreakerRegistry(CircuitBreakerRegistry circuitBreakerRegistry) {
        this.circuitBreakerRegistry = circuitBreakerRegistry;
    }

    @Bean
    @Scope(scopeName = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public Feign.Builder countryStateCityApiClientFeignBuilder() {
        CircuitBreaker circuitBreaker = circuitBreakerRegistry.circuitBreaker(CountryStateCityApiClient.SERVICE_CLIENT_NAME);
        FeignDecorators decorators = FeignDecorators.builder()
                .withCircuitBreaker(circuitBreaker)
                .withFallback(countryStateCityApiClientFallback, CallNotPermittedException.class)
                .build();
        return Resilience4jFeign.builder(decorators);
    }

}
