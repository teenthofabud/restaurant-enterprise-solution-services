package com.teenthofabud.restaurant.solution.customer.address.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountException;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.customer.account.service.AccountService;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressDto;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class AddressDtoValidator implements Validator {

    private List<String> fieldsToEscape;

    @Value("#{'${res.customer.address.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private AccountService accountService;

    @Autowired
    public void setAccountService(AccountService accountService) {
        this.accountService = accountService;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(AddressDto.class);
    }

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
        Optional<String> optAddressLine2 = dto.getAddressLine2();
        if(!fieldsToEscape.contains("addressLine2") && optAddressLine2.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optAddressLine2.get()))) {
            errors.rejectValue("addressLine2", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountDto.addressLine2 is invalid");
            return;
        }
        Optional<String> optStateId = dto.getStateId();
        if(!fieldsToEscape.contains("stateId") && optStateId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optStateId.get()))) {
            errors.rejectValue("stateId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountDto.stateId is invalid");
            return;
        }
        Optional<String> optPincode = dto.getPincode();
        if(!fieldsToEscape.contains("pincode") && optPincode.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optPincode.get()))) {
            errors.rejectValue("pincode", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountDto.pincode is invalid");
            return;
        }
        Optional<String> optCityId = dto.getCityId();
        if(!fieldsToEscape.contains("cityId") && optCityId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optCityId.get()))) {
            errors.rejectValue("cityId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountDto.cityId is invalid");
            return;
        }
        Optional<String> optCountryId = dto.getCountryId();
        if(!fieldsToEscape.contains("countryId") && optCountryId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optCountryId.get()))) {
            errors.rejectValue("countryId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountDto.countryId is invalid");
            return;
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
    }

}
