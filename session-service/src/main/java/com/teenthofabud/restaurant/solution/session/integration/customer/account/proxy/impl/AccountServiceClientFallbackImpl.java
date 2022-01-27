package com.teenthofabud.restaurant.solution.session.integration.customer.account.proxy.impl;

import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.session.integration.customer.account.proxy.AccountServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component("accountServiceClientFallback")
@Slf4j
public class AccountServiceClientFallbackImpl implements AccountServiceClient {

    @Override
    public AccountVo getAccountDetailsById(String id) {
        log.debug("Falling back to default implementation of getting account details by id");
        return new AccountVo();
    }
}
