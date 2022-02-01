package com.teenthofabud.restaurant.solution.customer.health;

import com.teenthofabud.restaurant.solution.customer.integration.metadata.data.HealthVo;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.proxy.MetadataServiceClient;
import feign.FeignException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.boot.actuate.health.Status;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Random;

@Slf4j
@Component
public class MetadataServiceIntegrationHealthIndicator implements HealthIndicator {

    private MetadataServiceClient metadataServiceClient;

    @Autowired
    public void setItemServiceClient(MetadataServiceClient metadataServiceClient) {
        this.metadataServiceClient = metadataServiceClient;
    }

    private String selectStatus() {
        List<Status> collection = Arrays.asList(Status.UP, Status.DOWN);
        Collections.shuffle(collection, new Random(System.currentTimeMillis()));
        Status selected = collection.get(0);
        return selected.getCode();
    }

    @Override
    public Health health() {
        String key = MetadataServiceClient.SERVICE_CLIENT_NAME;
        Status status = Status.DOWN;
        String value = "Unavailable";
        try {
            HealthVo inventoryHealth = this.metadataServiceClient.health(this.selectStatus());
            if(inventoryHealth.getStatus().toUpperCase().equals(Status.UP.getCode())) {
                status = Status.UP;
                value = "Available";
            }
        } catch (FeignException e) {
            log.error("Unable to query health of " + key, e);
        }
        Health health = Health.status(status).withDetail(key, value).build();
        return health;
    }
}
