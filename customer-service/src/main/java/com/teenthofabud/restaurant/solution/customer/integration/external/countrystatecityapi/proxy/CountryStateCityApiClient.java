package com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.proxy;

import com.teenthofabud.core.common.marker.TOABFeignErrorHandler;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.configuration.ExternalServiceCountryStateCityApiIntegrationConfiguration;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.data.CityVo;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.data.CountryVo;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.data.StateVo;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.error.CountryStateCityApiClientExceptionHandler;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

import java.util.List;

@FeignClient(value = CountryStateCityApiClient.SERVICE_CLIENT_NAME, url = "${customer.external.country-state-city-api.url}", configuration = ExternalServiceCountryStateCityApiIntegrationConfiguration.class)
public interface CountryStateCityApiClient {

    public static final String SERVICE_CLIENT_NAME = "external-service-country-state-city-api-client";

    @GetMapping("/{ciso}")
    @TOABFeignErrorHandler(CountryStateCityApiClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public CountryVo getCountryDetailsFromISO2Code(@PathVariable(required = true) String ciso);


    @GetMapping("/{ciso}/states/{siso}")
    @TOABFeignErrorHandler(CountryStateCityApiClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public StateVo getTheStateDetailsFromISO2Code(@PathVariable(required = true) String ciso, @PathVariable(required = true) String siso);

    @GetMapping("/{ciso}/cities")
    @TOABFeignErrorHandler(CountryStateCityApiClientExceptionHandler.class)
    @CircuitBreaker(name = SERVICE_CLIENT_NAME)
    public List<CityVo> getTheListOfCitiesInACountry(@PathVariable(required = true) String ciso);

}