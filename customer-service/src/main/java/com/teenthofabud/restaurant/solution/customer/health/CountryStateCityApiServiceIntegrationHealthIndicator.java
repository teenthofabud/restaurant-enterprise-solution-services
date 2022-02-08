package com.teenthofabud.restaurant.solution.customer.health;

import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.CountryVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.proxy.CountryStateCityApiServiceClient;
import feign.FeignException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.boot.actuate.health.Status;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
public class CountryStateCityApiServiceIntegrationHealthIndicator implements HealthIndicator {

    private CountryStateCityApiServiceClient countryStateCityApiServiceClient;

    @Autowired
    public void setProductServiceClient(CountryStateCityApiServiceClient countryStateCityApiServiceClient) {
        this.countryStateCityApiServiceClient = countryStateCityApiServiceClient;
    }

    @Override
    public Health health() {
        String key = CountryStateCityApiServiceClient.SERVICE_CLIENT_NAME;
        Status status = Status.DOWN;
        String value = "Unavailable";
        try {
            List<CountryVo> countryVoList = this.countryStateCityApiServiceClient.getAllCountries();
            if(!CollectionUtils.isEmpty(countryVoList)) {
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
