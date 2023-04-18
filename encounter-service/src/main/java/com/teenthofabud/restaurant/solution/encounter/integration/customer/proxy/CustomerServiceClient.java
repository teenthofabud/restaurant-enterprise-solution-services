package com.teenthofabud.restaurant.solution.encounter.integration.customer.proxy;

import com.teenthofabud.core.common.data.vo.HealthVo;
import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.restaurant.solution.encounter.integration.customer.error.CustomerServiceClientExceptionHandler;
import com.teenthofabud.restaurant.solution.encounter.integration.customer.configuration.CustomerServiceIntegrationConfiguration;
import com.teenthofabud.restaurant.solution.encounter.integration.customer.data.AccountVo;
import com.teenthofabud.restaurant.solution.encounter.integration.customer.error.CustomerServiceClientExceptionHandler;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(value = CustomerServiceClient.SERVICE_CLIENT_NAME, url = "${res.encounter.customer.service.url}", path = "/customer", configuration = CustomerServiceIntegrationConfiguration.class)
public interface CustomerServiceClient {

    public static final String SERVICE_CLIENT_NAME = "customer-service";


    @GetMapping("/account/{id}")
    @TOABFeignErrorHandler(CustomerServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public AccountVo getAccountDetailsById(@PathVariable(required = true) String id);


    @GetMapping("/account/{id}")
    @TOABFeignErrorHandler(CustomerServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public AccountVo getAccountDetailsById(@PathVariable(required = true) String id, @RequestParam String cascadeUntilLevel);

    @GetMapping("/actuator/health")
    @TOABFeignErrorHandler(CustomerServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public HealthVo health(@RequestParam(required = false) String status);

}