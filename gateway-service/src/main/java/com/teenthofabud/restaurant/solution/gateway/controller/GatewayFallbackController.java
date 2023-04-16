package com.teenthofabud.restaurant.solution.gateway.controller;

import org.springframework.cloud.gateway.route.RouteLocator;
import org.springframework.cloud.gateway.route.builder.RouteLocatorBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class GatewayFallbackController {

    @GetMapping("/establishmentAreaServiceFallBack")
    public String establishmentServiceFallBackMethod(){
        return "establishmentAreaService is taking longer than expected, please try again later!!!";
    }

    @GetMapping("/cookbookServiceFallBack")
    public String cookbookServiceFallBackMethod(){
        return "cookbookService is taking longer than expected, please try again later!!!";
    }

    @GetMapping("/configurationServiceFallBack")
    public String configurationServiceFallBackMethod(){
        return "configurationService is taking longer than expected, please try again later!!!";
    }

    @GetMapping("/inventoryServiceFallBack")
    public String inventoryServiceFallBackMethod(){
        return "inventoryService is taking longer than expected, please try again later!!!";
    }

    @GetMapping("/menuServiceFallBack")
    public String menuServiceFallBackMethod(){
        return "menuService is taking longer than expected, please try again later!!!";
    }

    @GetMapping("/bookingServiceFallBack")
    public String bookingServiceFallBackMethod(){
        return "bookingService is taking longer than expected, please try again later!!!";
    }

    @GetMapping("/settingsServiceFallBack")
    public String settingsServiceFallBackMethod(){
        return "settingsService is taking longer than expected, please try again later!!!";
    }

    @GetMapping("/customerServiceFallBack")
    public String customerServiceFallBackMethod(){
        return "customerService is taking longer than expected, please try again later!!!";
    }

    @GetMapping("/encounterServiceFallBack")
    public String encounterServiceFallBack(){
        return "encounterService is taking longer than expected, please try again later!!!";
    }

    @GetMapping("/engagementServiceFallBack")
    public String engagementServiceFallBack(){
        return "engagementService is taking longer than expected, please try again later!!!";
    }

//    @Bean
//    public KeyResolver ipKeyResolver() {
//        return exchange -> Mono.just(exchange.getRequest().getRemoteAddress().getAddress().getHostAddress());
//    }
//
//    @Bean
//    @Primary
//    public KeyResolver apiKeyResolver() {
//        return exchange -> Mono.just(exchange.getRequest().getPath().pathWithinApplication().value());
//    }
}
