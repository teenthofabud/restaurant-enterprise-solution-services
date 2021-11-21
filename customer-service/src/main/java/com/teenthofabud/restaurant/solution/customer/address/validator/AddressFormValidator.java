package com.teenthofabud.restaurant.solution.customer.address.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.dto.TOABValidationContextHolder;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountException;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.customer.account.service.AccountService;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressConstant;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressForm;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class AddressFormValidator implements Validator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.customer.address.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private AccountService accountService;
    private Validator countryIdValidator;
    private Validator stateIdValidator;
    private Validator cityIdValidator;

    @Autowired
    public void setAccountService(AccountService accountService) {
        this.accountService = accountService;
    }

    @Autowired
    public void setCountryIdValidator(Validator countryIdValidator) {
        this.countryIdValidator = countryIdValidator;
    }

    @Autowired
    public void setStateIdValidator(Validator stateIdValidator) {
        this.stateIdValidator = stateIdValidator;
    }

    @Autowired
    public void setCityIdValidator(Validator cityIdValidator) {
        this.cityIdValidator = cityIdValidator;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(AddressForm.class);
    }

    /**
     * Order in which the fields are being validated are important
     */

    @Override
    public void validate(Object target, Errors errors) {
        AddressForm form = (AddressForm) target;
        if(!fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("AddressForm.name is empty");
            errors.rejectValue("name", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        log.debug("AddressForm.name is valid");
        if(!fieldsToEscape.contains("addressLine1") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAddressLine1()))) {
            log.debug("AddressForm.addressLine1 is empty");
            errors.rejectValue("addressLine1", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        log.debug("AddressForm.addressLine1 is valid");
        /*if(!fieldsToEscape.contains("addressLine2") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAddressLine2()))) {
            log.debug("AddressForm.addressLine2 is empty");
            errors.rejectValue("addressLine2", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }*/
        if(!fieldsToEscape.contains("countryId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCountryId()))) {
            log.debug("AddressForm.countryId is empty");
            errors.rejectValue("countryId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("countryId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCountryId()))){
            Errors err = new DirectFieldBindingResult(form.getCountryId(), "AddressForm");
            countryIdValidator.validate(form.getCountryId(), err);
            if(err.hasErrors()) {
                log.debug("AddressForm.countryId is invalid");
                CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AddressForm error detail: {}", ec);
                errors.rejectValue("countryId", ec.name());
                return;
            }
            TOABValidationContextHolder.setSupportingValidationParameterContext(AddressConstant.COUNTRY_ISO.getName(), form.getCountryId());
        }
        log.debug("AddressForm.countryId is valid");
        if(!fieldsToEscape.contains("cityId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCityId()))) {
            log.debug("AddressForm.cityId is empty");
            errors.rejectValue("cityId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("cityId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCityId()))){
            Errors err = new DirectFieldBindingResult(form.getCityId(), "AddressForm");
            cityIdValidator.validate(form.getCityId(), err);
            if(err.hasErrors()) {
                log.debug("AddressForm.cityId is invalid");
                CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AddressForm error detail: {}", ec);
                errors.rejectValue("cityId", ec.name());
                return;
            }
        }
        log.debug("AddressForm.cityId is valid");
        if(!fieldsToEscape.contains("stateId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getStateId()))) {
            log.debug("AddressForm.stateId is empty");
            errors.rejectValue("stateId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("stateId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getStateId()))){
            Errors err = new DirectFieldBindingResult(form.getStateId(), "AddressForm");
            stateIdValidator.validate(form.getStateId(), err);
            if(err.hasErrors()) {
                log.debug("AddressForm.stateId is invalid");
                CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AddressForm error detail: {}", ec);
                errors.rejectValue("stateId", ec.name());
                return;
            }
        }
        log.debug("AddressForm.stateId is valid");
        if(!fieldsToEscape.contains("pincode") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getPincode()))) {
            errors.rejectValue("pincode", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AddressForm.pincode is invalid");
            return;
        }
        log.debug("AddressForm.pincode is valid");
        if(!fieldsToEscape.contains("accountId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAccountId()))) {
            log.debug("AddressForm.accountId is empty");
            errors.rejectValue("accountId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("accountId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))){
            String accountId = form.getAccountId();
            try {
                AccountVo accountVo = accountService.retrieveDetailsById(accountId, Optional.of(TOABCascadeLevel.ONE));
                if(!accountVo.getActive()) {
                    log.debug("AddressForm.accountId is inactive");
                    errors.rejectValue("accountId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (AccountException e) {
                log.debug("AddressForm.accountId is invalid");
                errors.rejectValue("accountId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("AddressForm.accountId is valid");
        TOABValidationContextHolder.clearSupportingValidationParameterContext(AddressConstant.COUNTRY_ISO.getName());
    }

}
