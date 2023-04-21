package com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.proxy;

import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.configuration.CountryStateCityApiServiceIntegrationConfiguration;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.CityVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.CountryVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.StateVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.error.CountryStateCityApiServiceClientExceptionHandler;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

import java.util.List;

@FeignClient(value = CountryStateCityApiServiceClient.SERVICE_CLIENT_NAME, url = "${res.customer.country-state-city-api.service.url}", configuration = CountryStateCityApiServiceIntegrationConfiguration.class)
public interface CountryStateCityApiServiceClient {

    public static final String SERVICE_CLIENT_NAME = "country-state-city-api-service";

    @GetMapping("/{ciso}")
    @TOABFeignErrorHandler(CountryStateCityApiServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    @CachePut(cacheNames = "countries", key = "#ciso", cacheManager = "cacheManager")
    public CountryVo getCountryDetailsFromISO2Code(@PathVariable(required = true) String ciso);


    @GetMapping("/{ciso}/states/{siso}")
    @TOABFeignErrorHandler(CountryStateCityApiServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    @CachePut(cacheNames = "states", key = "#siso", cacheManager = "cacheManager")
    public StateVo getTheStateDetailsFromISO2Code(@PathVariable(required = true) String ciso, @PathVariable(required = true) String siso);

    @GetMapping("/{ciso}/cities")
    @TOABFeignErrorHandler(CountryStateCityApiServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    @CachePut(cacheNames = "cities", key = "#ciso", cacheManager = "cacheManager")
    public List<CityVo> getTheListOfCitiesInACountry(@PathVariable(required = true) String ciso);

    @GetMapping
    @TOABFeignErrorHandler(CountryStateCityApiServiceClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public List<CountryVo> getAllCountries();

}