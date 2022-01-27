package com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.proxy;

import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.configuration.EstablishmentAreaServiceTableIntegrationConfiguration;
import com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.data.TableVo;
import com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.error.TableServiceClientExceptionHandler;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = TableServiceClient.SERVICE_CLIENT_NAME, url = "${cookbook.establishmentarea.table-service.url}", configuration = EstablishmentAreaServiceTableIntegrationConfiguration.class)
public interface TableServiceClient {

    public static final String SERVICE_CLIENT_NAME = "establishmentarea-service-table-client";

    @GetMapping("/{id}")
    @TOABFeignErrorHandler(TableServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public TableVo getTableDetailsById(@PathVariable(required = true) String id);

}