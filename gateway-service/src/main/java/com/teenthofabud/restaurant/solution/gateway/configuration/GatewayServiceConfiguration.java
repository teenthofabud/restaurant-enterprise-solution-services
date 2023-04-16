package com.teenthofabud.restaurant.solution.gateway.configuration;

import org.springframework.cloud.gateway.route.RouteLocator;
import org.springframework.cloud.gateway.route.builder.RouteLocatorBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class GatewayServiceConfiguration {

    @Bean
    public RouteLocator gatewayRoutes(RouteLocatorBuilder builder) {
        return builder.routes()
                .route("establishment-area-service", r -> r.path("/establishment-area/**")
                        .filters(f -> f.circuitBreaker(c -> c.setName("establishment-area").setFallbackUri("forward:/establishmentAreaServiceFallBack"))
                                        .rewritePath("/establishment-area/(?<remaining>.*)", "/${remaining}")
                                /*.requestRateLimiter(a -> a.setKeyResolver("#{@apiKeyResolver})").*/)
                        .uri("lb://establishment-area-service"))

                .route("configuration-service", r -> r.path("/configuration/**")
                        .filters(f -> f.circuitBreaker(c -> c.setName("configuration").setFallbackUri("forward:/configurationServiceFallBack"))
                                .rewritePath("/configuration/(?<remaining>.*)", "/${remaining}"))
                        .uri("lb://configuration-service"))

                .route("cookbook-service", r -> r.path("/cookbook/**")
                        .filters(f -> f.circuitBreaker(c -> c.setName("cookbook").setFallbackUri("forward:/cookbookServiceFallBack"))
                                .rewritePath("/cookbook/(?<remaining>.*)", "/${remaining}"))
                        .uri("lb://cookbook-service"))

                .route("inventory-service", r -> r.path("/inventory/**")
                        .filters(f -> f.circuitBreaker(c -> c.setName("inventory").setFallbackUri("forward:/inventoryServiceFallBack"))
                                .rewritePath("/inventory/(?<remaining>.*)", "/${remaining}"))
                        .uri("lb://inventory-service"))

                .route("menu-service", r -> r.path("/menu/**")
                        .filters(f -> f.circuitBreaker(c -> c.setName("menu").setFallbackUri("forward:/menuServiceFallBack"))
                                .rewritePath("/menu/(?<remaining>.*)", "/${remaining}"))
                        .uri("lb://menu-service"))

                .route("booking-service", r -> r.path("/booking/**")
                        .filters(f -> f.circuitBreaker(c -> c.setName("booking").setFallbackUri("forward:/bookingServiceFallBack"))
                                .rewritePath("/booking/(?<remaining>.*)", "/${remaining}"))
                        .uri("lb://booking-service"))

                .route("settings-service", r -> r.path("/settings/**")
                        .filters(f -> f.circuitBreaker(c -> c.setName("settings").setFallbackUri("forward:/settingsServiceFallBack"))
                                .rewritePath("/settings/(?<remaining>.*)", "/${remaining}"))
                        .uri("lb://settings-service"))

                .route("customer-service", r -> r.path("/customer/**")
                        .filters(f -> f.circuitBreaker(c -> c.setName("customer").setFallbackUri("forward:/customerServiceFallBack"))
                                .rewritePath("/customer/(?<remaining>.*)", "/${remaining}"))
                        .uri("lb://customer-service"))

                .route("encounter-service", r -> r.path("/encounter/**")
                        .filters(f -> f.circuitBreaker(c -> c.setName("encounter").setFallbackUri("forward:/encounterServiceFallBack"))
                                .rewritePath("/encounter/(?<remaining>.*)", "/${remaining}"))
                        .uri("lb://encounter-service"))

                .route("engagement-service", r -> r.path("/engagement/**")
                        .filters(f -> f.circuitBreaker(c -> c.setName("engagement").setFallbackUri("forward:/engagementServiceFallBack"))
                                .rewritePath("/engagement/(?<remaining>.*)", "/${remaining}"))
                        .uri("lb://engagement-service"))
                .build();
    }
}
