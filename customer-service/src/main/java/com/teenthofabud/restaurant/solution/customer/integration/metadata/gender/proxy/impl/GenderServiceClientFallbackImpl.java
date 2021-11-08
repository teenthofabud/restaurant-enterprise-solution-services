package com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.proxy.impl;

import com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.data.GenderVo;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.proxy.GenderServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component("genderServiceClientFallback")
@Slf4j
public class GenderServiceClientFallbackImpl implements GenderServiceClient {

    @Override
    public GenderVo getGenderDetailsById(String id) {
        log.debug("Falling back to default implementation of getting ethnicity details by id");
        return new GenderVo();
    }
}
