package com.teenthofabud.restaurant.solution.reservation.integration.establishmentarea.proxy.impl;

import com.teenthofabud.core.common.data.vo.HealthVo;
import com.teenthofabud.restaurant.solution.reservation.integration.establishmentarea.data.TableVo;
import com.teenthofabud.restaurant.solution.reservation.integration.establishmentarea.proxy.EstablishmentAreaServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component("establishmentAreaServiceClientFallback")
@Slf4j
public class EstablishmentAreaServiceClientFallbackImpl implements EstablishmentAreaServiceClient {

    @Override
    public TableVo getTableDetailsById(String id) {
        log.debug("Falling back to default implementation of getting table details by id");
        return new TableVo();
    }

    @Override
    public HealthVo health(String status) {
        log.debug("Falling back to default implementation of querying health status: {}", status);
        return new HealthVo();
    }
}
