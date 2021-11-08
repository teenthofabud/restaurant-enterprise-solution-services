package com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.proxy;

import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.configuration.MetadataServiceGenderIntegrationConfiguration;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.data.GenderVo;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.error.GenderServiceClientExceptionHandler;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = GenderServiceClient.SERVICE_CLIENT_NAME, url = "${customer.metadata.gender-service.url}", configuration = MetadataServiceGenderIntegrationConfiguration.class)
public interface GenderServiceClient {

    public static final String SERVICE_CLIENT_NAME = "metadata-service-gender-client";

    @GetMapping("/{id}")
    @TOABFeignErrorHandler(GenderServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public GenderVo getGenderDetailsById(@PathVariable(required = true) String id);

}