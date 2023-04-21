package com.teenthofabud.restaurant.solution.customer.integration.metadata.proxy;

import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.data.HealthVo;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.configuration.MetadataServiceIntegrationConfiguration;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.data.GenderVo;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.error.MetadataServiceClientExceptionHandler;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(value = MetadataServiceClient.SERVICE_CLIENT_NAME, url = "${res.customer.metadata.service.url}", path = "/metadata", configuration = MetadataServiceIntegrationConfiguration.class)
public interface MetadataServiceClient {

    public static final String SERVICE_CLIENT_NAME = "metadata-service";

    @GetMapping("/gender/{id}")
    @TOABFeignErrorHandler(MetadataServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public GenderVo getGenderDetailsById(@PathVariable(required = true) String id);

    @GetMapping("/actuator/health")
    @TOABFeignErrorHandler(MetadataServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public HealthVo health(@RequestParam(required = false) String status);

}