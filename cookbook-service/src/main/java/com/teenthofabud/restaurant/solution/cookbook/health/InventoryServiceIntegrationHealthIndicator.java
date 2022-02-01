package com.teenthofabud.restaurant.solution.cookbook.health;

import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.data.HealthVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.proxy.InventoryServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.boot.actuate.health.Status;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class InventoryServiceIntegrationHealthIndicator implements HealthIndicator {

    private InventoryServiceClient inventoryServiceClient;

    @Autowired
    public void setProductServiceClient(InventoryServiceClient inventoryServiceClient) {
        this.inventoryServiceClient = inventoryServiceClient;
    }

    @Override
    public Health health() {
        HealthVo inventoryHealth = this.inventoryServiceClient.health();
        String key = InventoryServiceClient.SERVICE_CLIENT_NAME;
        Status status = null;
        String value = "";
        if(inventoryHealth.getStatus().toUpperCase().equals(Status.UP.getCode())) {
            status = Status.UP;
            value = "Available";
        } else {
            status = Status.DOWN;
            value = "Unavailable";
        }
        Health health = Health.status(status).withDetail(key, value).build();
        return health;
    }
}
