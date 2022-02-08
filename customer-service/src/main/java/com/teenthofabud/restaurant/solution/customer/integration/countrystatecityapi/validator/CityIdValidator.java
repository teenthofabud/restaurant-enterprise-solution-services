package com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.validator;

import com.teenthofabud.core.common.data.dto.TOABValidationContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.CityVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.service.CountryStateCityApiService;
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

    private CountryStateCityApiService countryStateCityApiService;

    @Autowired
    public void setCountryStateCityApiService(CountryStateCityApiService countryStateCityApiService) {
        this.countryStateCityApiService = countryStateCityApiService;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(String.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        String objectName = errors.getObjectName();
        String cityIdStr = (String) target;
        Long cityId = 0L;
        try {
            cityId = Long.parseLong(cityIdStr);
        } catch (NumberFormatException e) {
            log.debug(objectName + ".city.id is invalid");
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        Optional<Object> optionalCountryIso = TOABValidationContextHolder.getSupportingValidationParameterContext("countryIso");
        if(optionalCountryIso.isEmpty()) {
            throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "countryIso is required to get state details", new Object[] { "country iso is required" });
        }
        String countryIso =  optionalCountryIso.get().toString();
        log.debug("Validating city id: {}", cityId);
        List<CityVo> cities = new LinkedList<>();
        log.info("Requesting details of city with id: {} for country with iso: {}", cityId, countryIso);
        cities = countryStateCityApiService.getTheListOfCitiesInACountry(countryIso, countryIso);
        log.info("Retrieved cities: {} by iso for country with iso: {}", cities, countryIso);
        if(cities == null || cities.isEmpty()) {
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".country.iso is invalid");
            return;
        }
        Long finalCityId = cityId;
        Optional<CityVo> optionalCityVo = cities.stream().filter(c -> c.getId().compareTo(finalCityId) == 0).findAny();
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
