package com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.service.impl;

import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.CityVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.CountryVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.StateVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.proxy.CountryStateCityApiServiceClient;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.service.CountryStateCityApiService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;

@Slf4j
@Service
@CacheConfig(cacheNames = { "countries", "states", "cities" })
public class CountryStateCityApiServiceImpl implements CountryStateCityApiService {

    private CountryStateCityApiServiceClient countryStateCityApiServiceClient;

    @Autowired
    public void setCountryStateCityApiClient(CountryStateCityApiServiceClient countryStateCityApiServiceClient) {
        this.countryStateCityApiServiceClient = countryStateCityApiServiceClient;
    }

    @Cacheable(cacheNames = "countries", key = "#countryId")
    @Override
    public CountryVo getCountryDetailsFromISO2Code(String countryId, String ciso) {
        CountryVo countryVo = null;
        log.info("Requesting details of country with iso: {}", ciso);
        countryVo = countryStateCityApiServiceClient.getCountryDetailsFromISO2Code(ciso);
        log.info("Retrieved country: {} by iso", countryVo);
        return countryVo;
    }

    @Cacheable(cacheNames = "states", key = "#countryIdStateId")
    @Override
    public StateVo getTheStateDetailsFromISO2Code(String countryIdStateId, String ciso, String siso) {
        StateVo stateVo = null;
        log.info("Requesting details of state with iso: {} for country with iso: {}", siso, ciso);
        stateVo = countryStateCityApiServiceClient.getTheStateDetailsFromISO2Code(ciso, siso);
        log.info("Retrieved state: {} by iso for country with iso: {}", stateVo, ciso);
        return stateVo;
    }

    @Cacheable(cacheNames = "cities", key = "#countryId")
    @Override
    public List<CityVo> getTheListOfCitiesInACountry(String countryId, String ciso) {
        List<CityVo> cities = new LinkedList<>();
        log.info("Requesting details of cities in country with iso: {}", ciso);
        cities = countryStateCityApiServiceClient.getTheListOfCitiesInACountry(ciso);
        log.info("Retrieved cities: {} by iso for country with iso: {}", cities, ciso);
        return cities;
    }
}
