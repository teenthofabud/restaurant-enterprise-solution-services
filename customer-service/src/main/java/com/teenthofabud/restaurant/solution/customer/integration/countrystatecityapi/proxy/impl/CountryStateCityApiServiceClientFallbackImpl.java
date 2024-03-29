package com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.proxy.impl;

import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.CountryVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.HealthVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.StateVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.CityVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.proxy.CountryStateCityApiServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.LinkedList;
import java.util.List;

@Component("countryStateCityApiClientFallback")
@Slf4j
public class CountryStateCityApiServiceClientFallbackImpl implements CountryStateCityApiServiceClient {

    @Override
    public CountryVo getCountryDetailsFromISO2Code(String iso2) {
        log.debug("Falling back to default implementation of getting country details from iso2 code");
        return new CountryVo();
    }

    @Override
    public StateVo getTheStateDetailsFromISO2Code(String ciso, String siso) {
        log.debug("Falling back to default implementation of getting state details from iso2 code");
        return new StateVo();
    }

    @Override
    public List<CityVo> getTheListOfCitiesInACountry(String ciso) {
        log.debug("Falling back to default implementation of getting state details from iso2 code");
        return new LinkedList<>();
    }

    @Override
    public List<CountryVo> getAllCountries() {
        log.debug("Falling back to default implementation of querying all cities");
        return new LinkedList<>();
    }

}
