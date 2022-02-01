package com.teenthofabud.restaurant.solution.cookbook.health;

import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.data.HealthVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.proxy.MenuServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.boot.actuate.health.Status;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class MenuServiceIntegrationHealthIndicator implements HealthIndicator {

    private MenuServiceClient menuServiceClient;

    @Autowired
    public void setItemServiceClient(MenuServiceClient menuServiceClient) {
        this.menuServiceClient = menuServiceClient;
    }

    @Override
    public Health health() {
        HealthVo inventoryHealth = this.menuServiceClient.health();
        String key = MenuServiceClient.SERVICE_CLIENT_NAME;
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
