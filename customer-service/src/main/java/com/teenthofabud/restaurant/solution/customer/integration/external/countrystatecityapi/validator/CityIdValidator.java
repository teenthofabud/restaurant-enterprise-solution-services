package com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.validator;

import com.teenthofabud.core.common.data.dto.TOABValidationContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.data.CityVo;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.proxy.CountryStateCityApiClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

@Component("cityIdValidator")
@Slf4j
public class CityIdValidator implements Validator {

    private CountryStateCityApiClient countryStateCityApiClient;

    @Autowired
    public void setCountryStateCityApiClient(CountryStateCityApiClient countryStateCityApiClient) {
        this.countryStateCityApiClient = countryStateCityApiClient;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(String.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        String objectName = errors.getObjectName();
        Long cityId = (Long) target;
        Optional<Object> optionalCountryIso = TOABValidationContextHolder.getSupportingValidationParameterContext("countryIso");
        if(optionalCountryIso.isEmpty()) {
            throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "countryIso is required to get city details");
        }
        String countryIso =  optionalCountryIso.get().toString();
        log.debug("Validating city id: {}", cityId);
        List<CityVo> cities = new LinkedList<>();
        log.info("Requesting details of city with id: {} for country with iso: {}", cityId, countryIso);
        cities = countryStateCityApiClient.getTheListOfCitiesInACountry(countryIso);
        log.info("Retrieved cities: {} by iso for country with iso: {}", cities, countryIso);
        if(cities == null || cities.isEmpty()) {
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".country.iso is invalid");
            return;
        }
        Optional<CityVo> optionalCityVo = cities.stream().filter(c -> c.getId().compareTo(cityId) == 0).findAny();
        if(optionalCityVo.isEmpty()) {
            log.debug(objectName + ".city.id is invalid");
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        CityVo cityVo = optionalCityVo.get();
        boolean emptyName = !StringUtils.hasText(StringUtils.trimWhitespace(cityVo.getName()));
        boolean emptyId = cityVo.getId() == null || cityVo.getId() <= 0;
        if(emptyName) {
            log.debug(objectName + ".city.name is invalid");
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(emptyId) {
            log.debug(objectName + ".city.id is invalid");
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
    }

}