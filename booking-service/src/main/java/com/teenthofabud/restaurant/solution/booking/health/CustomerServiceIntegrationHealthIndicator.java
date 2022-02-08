package com.teenthofabud.restaurant.solution.booking.health;

import com.teenthofabud.core.common.data.vo.HealthVo;
import com.teenthofabud.core.common.health.TOABBaseServiceHealthIndicator;
import com.teenthofabud.restaurant.solution.booking.integration.customer.proxy.CustomerServiceClient;
import feign.FeignException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.Status;
import org.springframework.stereotype.Component;


@Slf4j
@Component
public class CustomerServiceIntegrationHealthIndicator extends TOABBaseServiceHealthIndicator {

    private CustomerServiceClient customerServiceClient;

    @Autowired
    public void setProductServiceClient(CustomerServiceClient customerServiceClient) {
        this.customerServiceClient = customerServiceClient;
    }

    @Override
    public String getServiceName() {
        return CustomerServiceClient.SERVICE_CLIENT_NAME;
    }

    @Override
    public Status getServiceStatus() throws FeignException {
        HealthVo inventoryHealth = this.customerServiceClient.health(this.selectStatus());
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
