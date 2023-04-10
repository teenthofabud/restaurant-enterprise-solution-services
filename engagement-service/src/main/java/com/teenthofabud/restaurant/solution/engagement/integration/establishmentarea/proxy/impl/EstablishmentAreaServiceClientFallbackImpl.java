package com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.proxy.impl;

import com.teenthofabud.core.common.data.vo.HealthVo;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.data.TableVo;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.proxy.EstablishmentAreaServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.RequestParam;

@Component("establishmentAreaServiceClientFallback")
@Slf4j
public class EstablishmentAreaServiceClientFallbackImpl implements EstablishmentAreaServiceClient {

    @Override
    public TableVo getTableDetailsById(String id, @RequestParam String cascadeUntilLevel) {
        log.debug("Falling back to default implementation of getting table details by id and cascadeUntilLevel");
        return new TableVo();
    }

    @Override
    public HealthVo health(String status) {
        log.debug("Falling back to default implementation of querying health status: {}", status);
        return new HealthVo();
    }
}
