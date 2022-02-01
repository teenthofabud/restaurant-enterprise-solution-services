package com.teenthofabud.restaurant.solution.customer.integration.metadata.proxy.impl;

import com.teenthofabud.restaurant.solution.customer.integration.metadata.data.GenderVo;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.data.HealthVo;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.proxy.MetadataServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component("genderServiceClientFallback")
@Slf4j
public class MetadataServiceClientFallbackImpl implements MetadataServiceClient {

    @Override
    public GenderVo getGenderDetailsById(String id) {
        log.debug("Falling back to default implementation of getting ethnicity details by id: {}", id);
        return new GenderVo();
    }

    @Override
    public HealthVo health(String status) {
        log.debug("Falling back to default implementation of querying health status: {}", status);
        return new HealthVo();
    }

}
