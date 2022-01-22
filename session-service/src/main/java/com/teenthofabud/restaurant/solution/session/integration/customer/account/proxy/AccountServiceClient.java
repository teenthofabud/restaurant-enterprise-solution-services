package com.teenthofabud.restaurant.solution.session.integration.customer.account.proxy;

import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.session.integration.customer.account.configuration.CustomerServiceAccountIntegrationConfiguration;
import com.teenthofabud.restaurant.solution.session.integration.customer.account.error.AccountServiceClientExceptionHandler;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = AccountServiceClient.SERVICE_CLIENT_NAME, url = "${cookbook.customer.account-service.url}", configuration = CustomerServiceAccountIntegrationConfiguration.class)
public interface AccountServiceClient {

    public static final String SERVICE_CLIENT_NAME = "customer-service-account-client";

    @GetMapping("/{id}")
    @TOABFeignErrorHandler(AccountServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public AccountVo getAccountDetailsById(@PathVariable(required = true) String id);

}