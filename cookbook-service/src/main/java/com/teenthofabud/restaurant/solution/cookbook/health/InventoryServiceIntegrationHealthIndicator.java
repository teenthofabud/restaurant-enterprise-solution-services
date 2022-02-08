package com.teenthofabud.restaurant.solution.cookbook.health;

import com.teenthofabud.core.common.data.vo.HealthVo;
import com.teenthofabud.core.common.health.TOABBaseServiceHealthIndicator;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.proxy.InventoryServiceClient;
import feign.FeignException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.Status;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Random;

@Slf4j
@Component
public class InventoryServiceIntegrationHealthIndicator extends TOABBaseServiceHealthIndicator {

    private InventoryServiceClient inventoryServiceClient;

    @Autowired
    public void setProductServiceClient(InventoryServiceClient inventoryServiceClient) {
        this.inventoryServiceClient = inventoryServiceClient;
    }

    @Override
    public String getServiceName() {
        return null;
    }

    @Override
    public Status getServiceStatus() throws FeignException {
        HealthVo inventoryHealth = this.inventoryServiceClient.health(this.selectStatus());
        if(inventoryHealth.getStatus().toUpperCase().equals(Status.UP.getCode())) {
            return Status.UP;
        } else {
            return Status.DOWN;
        }
    }

    @Override
    public Health health() {
        return super.determineHealthOfService();
    }

}
