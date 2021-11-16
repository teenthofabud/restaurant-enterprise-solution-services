package com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.validator;

import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.data.CountryVo;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.proxy.CountryStateCityApiClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component("countryIdValidator")
@Slf4j
public class CountryISOValidator implements Validator {

    private CountryStateCityApiClient countryStateCityApiClient;

    @Autowired
    public void setGenderServiceClient(CountryStateCityApiClient countryStateCityApiClient) {
        this.countryStateCityApiClient = countryStateCityApiClient;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(String.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        String objectName = errors.getObjectName();
        String countryIso = (String) target;
        log.debug("Validating country iso: {}", countryIso);
        CountryVo countryVo = null;
        log.info("Requesting details of country with iso: {}", countryIso);
        countryVo = countryStateCityApiClient.getCountryDetailsFromISO2Code(countryIso);
        log.info("Retrieved country: {} by iso", countryVo);
        if(countryVo == null) {
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug(objectName + ".countryIso is invalid");
            return;
        }
        boolean emptyFileName = !StringUtils.hasText(StringUtils.trimWhitespace(countryVo.getName()));
        boolean emptyId = countryVo.getId() == null || countryVo.getId() <= 0;
        if(emptyFileName) {
            log.debug(objectName + ".country.name is invalid");
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(emptyId) {
            log.debug(objectName + ".country.iso is invalid");
            errors.reject(CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
    }

}
