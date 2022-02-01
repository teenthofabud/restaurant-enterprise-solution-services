package com.teenthofabud.restaurant.solution.customer.health;

import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.CountryVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.HealthVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.proxy.CountryStateCityApiServiceClient;
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
        List<CountryVo> countryVoList = this.countryStateCityApiServiceClient.getAllCountries();
        String key = CountryStateCityApiServiceClient.SERVICE_CLIENT_NAME;
        Status status = null;
        String value = "";
        if(!CollectionUtils.isEmpty(countryVoList)) {
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
