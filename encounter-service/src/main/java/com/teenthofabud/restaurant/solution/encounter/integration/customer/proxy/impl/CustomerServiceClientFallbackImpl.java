package com.teenthofabud.restaurant.solution.encounter.integration.customer.proxy.impl;

import com.teenthofabud.core.common.data.vo.HealthVo;
import com.teenthofabud.restaurant.solution.encounter.integration.customer.data.AccountVo;
import com.teenthofabud.restaurant.solution.encounter.integration.customer.proxy.CustomerServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component("customerServiceClientFallback")
@Slf4j
public class CustomerServiceClientFallbackImpl implements CustomerServiceClient {

    @Override
    public AccountVo getAccountDetailsById(String id) {
        log.debug("Falling back to default implementation of getting account details by id");
        return new AccountVo();
    }

    @Override
    public AccountVo getAccountDetailsById(String id, String cascadeUntilLevel) {
        log.debug("Falling back to default implementation of getting account details by id and cascadeUntilLevel");
        return new AccountVo();
    }

    @Override
    public HealthVo health(String status) {
        log.debug("Falling back to default implementation of querying health status: {}", status);
        return new HealthVo();
    }
}
