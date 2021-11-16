package com.teenthofabud.restaurant.solution.customer.address.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.dto.TOABValidationContextHolder;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountException;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.customer.account.service.AccountService;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressConstant;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressDto;
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
public class AddressDtoValidator implements Validator {

    private List<String> fieldsToEscape;
    private Validator countryIdValidator;
    private Validator stateIdValidator;
    private Validator cityIdValidator;

    @Value("#{'${res.customer.address.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private AccountService accountService;

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
        return clazz.isAssignableFrom(AddressDto.class);
    }

    /**
     * Order in which the fields are being validated are important
     */

    @Override
    public void validate(Object target, Errors errors) {
        AddressDto dto = (AddressDto) target;
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountDto.name is invalid");
            return;
        }
        Optional<String> optAddressLine1 = dto.getAddressLine1();
        if(!fieldsToEscape.contains("addressLine1") && optAddressLine1.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optAddressLine1.get()))) {
            errors.rejectValue("addressLine1", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountDto.addressLine1 is invalid");
            return;
        }
        /*Optional<String> optAddressLine2 = dto.getAddressLine2();
        if(!fieldsToEscape.contains("addressLine2") && optAddressLine2.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optAddressLine2.get()))) {
            errors.rejectValue("addressLine2", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountDto.addressLine2 is invalid");
            return;
        }*/
        Optional<String> optPincode = dto.getPincode();
        if(!fieldsToEscape.contains("pincode") && optPincode.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optPincode.get()))) {
            errors.rejectValue("pincode", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountDto.pincode is invalid");
            return;
        }
        Optional<String> optCountryId = dto.getCountryId();
        if(!fieldsToEscape.contains("countryId") && optCountryId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optCountryId.get()))) {
            String countryId = optCountryId.get();
            Errors err = new DirectFieldBindingResult(countryId, "AddressDto");
            countryIdValidator.validate(countryId, err);
            if(err.hasErrors()) {
                log.debug("AddressDto.countryId is invalid");
                CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AddressDto error detail: {}", ec);
                errors.rejectValue("countryId", ec.name());
                return;
            }
            TOABValidationContextHolder.setSupportingValidationParameterContext(AddressConstant.COUNTRY_ISO.getName(), countryId);
        }
        Optional<String> optCityId = dto.getCityId();
        if(!fieldsToEscape.contains("cityId") && optCityId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optCityId.get()))) {
            String cityId = optCityId.get();
            Errors err = new DirectFieldBindingResult(cityId, "AddressDto");
            cityIdValidator.validate(cityId, err);
            if(err.hasErrors()) {
                log.debug("AddressDto.cityId is invalid");
                CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AddressDto error detail: {}", ec);
                errors.rejectValue("cityId", ec.name());
                return;
            }
        }
        Optional<String> optStateId = dto.getStateId();
        if(!fieldsToEscape.contains("stateId") && optStateId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optStateId.get()))) {
            String stateId = optStateId.get();
            Errors err = new DirectFieldBindingResult(stateId, "AddressDto");
            stateIdValidator.validate(stateId, err);
            if(err.hasErrors()) {
                log.debug("AddressDto.stateId is invalid");
                CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AddressDto error detail: {}", ec);
                errors.rejectValue("stateId", ec.name());
                return;
            }
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
                log.debug("AccountDto.active is invalid");
                return;
            }
        }
        Optional<String> optAccountId = dto.getAccountId();
        if(!fieldsToEscape.contains("accountId") && optAccountId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optAccountId.get()))) {
            String accountId = optAccountId.get();
            try {
                AccountVo accountVo = accountService.retrieveDetailsById(accountId, Optional.of(TOABCascadeLevel.ONE));
                if(!accountVo.getActive()) {
                    log.debug("AccountDto.accountId is inactive");
                    errors.rejectValue("accountId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (AccountException e) {
                log.debug("AccountDto.accountId is invalid");
                errors.rejectValue("accountId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        TOABValidationContextHolder.clearSupportingValidationParameterContext(AddressConstant.COUNTRY_ISO.getName());
    }

}
