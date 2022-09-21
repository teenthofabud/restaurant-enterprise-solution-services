package com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.proxy;

import com.teenthofabud.core.common.data.vo.HealthVo;
import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.configuration.EstablishmentAreaServiceIntegrationConfiguration;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.data.TableVo;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.error.EstablishmentAreaServiceClientExceptionHandler;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(value = EstablishmentAreaServiceClient.SERVICE_CLIENT_NAME, url = "${reservation.establishmentarea.service.url}", configuration = EstablishmentAreaServiceIntegrationConfiguration.class)
public interface EstablishmentAreaServiceClient {

    public static final String SERVICE_CLIENT_NAME = "establishmentarea-service";

    @GetMapping("/establishmentarea/table/{id}")
    @TOABFeignErrorHandler(EstablishmentAreaServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public TableVo getTableDetailsById(@PathVariable(required = true) String id);

    @GetMapping("/actuator/health")
    @TOABFeignErrorHandler(EstablishmentAreaServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public HealthVo health(@RequestParam(required = false) String status);

}