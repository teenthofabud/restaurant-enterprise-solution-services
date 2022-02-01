package com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.validator;

import com.teenthofabud.core.common.data.dto.TOABValidationContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.data.StateVo;
import com.teenthofabud.restaurant.solution.customer.integration.countrystatecityapi.service.CountryStateCityApiService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Arrays;
import java.util.Optional;

@Component("stateIdValidator")
@Slf4j
public class StateISOValidator implements Validator {

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
        String stateIso = (String) target;
        Optional<Object> optionalCountryIso = TOABValidationContextHolder.getSupportingValidationParameterContext("countryIso");
        if(optionalCountryIso.isEmpty()) {
            throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "countryIso is required to get state details", new Object[] { "country iso is required" });
        }
        String countryIso =  optionalCountryIso.get().toString();
        log.debug("Validating state iso: {}", stateIso);
        StateVo stateVo = null;
        log.info("Requesting details of state with iso: {} for country with iso: {}", stateIso, countryIso);
        String countryIdStateId = String.join("-", Arrays.asList(countryIso, stateIso));
        stateVo = countryStateCityApiService.getTheStateDetailsFromISO2Code(countryIdStateId, countryIso, stateIso);
        log.info("Retrieved state: {} by iso for country with iso: {}", stateVo, countryIso);
        if(stateVo == null) {
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".stateIso is invalid");
            return;
        }
        boolean emptyFileName = !StringUtils.hasText(StringUtils.trimWhitespace(stateVo.getName()));
        boolean emptyId = !StringUtils.hasText(StringUtils.trimWhitespace(stateVo.getId()));
        if(emptyFileName) {
            log.debug(objectName + ".state.name is invalid");
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(emptyId) {
            log.debug(objectName + ".state.iso is invalid");
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
    }

}
