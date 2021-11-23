package com.teenthofabud.restaurant.solution.customer.address.validator;

import com.teenthofabud.core.common.data.dto.TOABValidationContextHolder;
import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressConstant;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressForm;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;

@Component
@Slf4j
public class AddressFormRelaxedValidator implements RelaxedValidator<AddressForm>  {

    private List<String> fieldsToEscape;

    @Value("#{'${res.customer.address.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private Validator countryIdValidator;
    private Validator stateIdValidator;
    private Validator cityIdValidator;

    @Autowired
    @Qualifier("countryIdValidator")
    public void setCountryIdValidator(Validator countryIdValidator) {
        this.countryIdValidator = countryIdValidator;
    }

    @Autowired
    @Qualifier("stateIdValidator")
    public void setStateIdValidator(Validator stateIdValidator) {
        this.stateIdValidator = stateIdValidator;
    }

    @Autowired
    @Qualifier("cityIdValidator")
    public void setCityIdValidator(Validator cityIdValidator) {
        this.cityIdValidator = cityIdValidator;
    }

    @Override
    public Boolean validateLoosely(AddressForm form, Errors errors) {
        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            errors.rejectValue("name", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AddressForm.name is empty");
            return false;
        }
        log.debug("AddressForm.name is valid");

        if(!fieldsToEscape.contains("addressLine1") && form.getAddressLine1() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAddressLine1()))) {
            errors.rejectValue("addressLine1", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AddressForm.addressLine1 is empty");
            return false;
        }
        log.debug("AddressForm.addressLine1 is valid");

        if(!fieldsToEscape.contains("countryId") && form.getCountryId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCountryId()))) {
            errors.rejectValue("countryId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AddressForm.countryId is empty");
            return false;
        } else if(!fieldsToEscape.contains("countryId") && form.getCountryId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getCountryId()))) {
            Errors err = new DirectFieldBindingResult(form.getCountryId(), "AddressForm");
            countryIdValidator.validate(form.getCountryId(), err);
            if(err.hasErrors()) {
                log.debug("AddressForm.countryId is invalid");
                CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AddressForm error detail: {}", ec);
                errors.rejectValue("countryId", ec.name());
                return false;
            }
            TOABValidationContextHolder.setSupportingValidationParameterContext(AddressConstant.COUNTRY_ISO.getName(), form.getCountryId());
        }
        log.debug("AddressForm.countryId is valid");

        if(!fieldsToEscape.contains("addressLine2") && form.getAddressLine2() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAddressLine2()))) {
            errors.rejectValue("addressLine2", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AddressForm.addressLine2 is empty");
            return false;
        }
        log.debug("AddressForm.addressLine2 is valid");

        if(!fieldsToEscape.contains("stateId") && form.getStateId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getStateId()))) {
            errors.rejectValue("stateId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AddressForm.stateId is empty");
            return false;
        } else if(!fieldsToEscape.contains("stateId") && form.getCityId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getStateId()))) {
            Errors err = new DirectFieldBindingResult(form.getStateId(), "AddressForm");
            stateIdValidator.validate(form.getStateId(), err);
            if(err.hasErrors()) {
                log.debug("AddressForm.stateId is invalid");
                CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AddressForm error detail: {}", ec);
                errors.rejectValue("stateId", ec.name());
                return false;
            }
        }
        log.debug("AddressForm.stateId is valid");

        if(!fieldsToEscape.contains("cityId") && form.getCityId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCityId()))) {
            errors.rejectValue("cityId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AddressForm.cityId is empty");
            return false;
        } else if(!fieldsToEscape.contains("cityId") && form.getCityId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getCityId()))) {
            Errors err = new DirectFieldBindingResult(form.getCityId(), "AddressForm");
            cityIdValidator.validate(form.getCityId(), err);
            if(err.hasErrors()) {
                log.debug("AddressForm.cityId is invalid");
                CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AddressForm error detail: {}", ec);
                errors.rejectValue("cityId", ec.name());
                return false;
            }
        }
        log.debug("AddressForm.cityId is valid");

        if(!fieldsToEscape.contains("accountId") && form.getAccountId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAccountId()))) {
            errors.rejectValue("accountId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AddressForm.accountId is empty");
            return false;
        }
        log.debug("AddressForm.accountId is valid");

        if(!fieldsToEscape.contains("pincode") && form.getPincode() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAccountId()))) {
            errors.rejectValue("pincode", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AddressForm.pincode is empty");
            return false;
        }
        log.debug("AddressForm.pincode is valid");
        return true;
    }
}
