package com.teenthofabud.restaurant.solution.customer.address.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountException;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.customer.account.service.AccountService;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressForm;
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
public class AddressFormValidator implements Validator {

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
        return clazz.isAssignableFrom(AddressForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        AddressForm form = (AddressForm) target;
        if(!fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("AccountForm.name is empty");
            errors.rejectValue("name", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("addressLine1") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAddressLine1()))) {
            log.debug("AccountForm.addressLine1 is empty");
            errors.rejectValue("addressLine1", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("addressLine2") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAddressLine2()))) {
            log.debug("AccountForm.addressLine2 is empty");
            errors.rejectValue("addressLine2", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("cityId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCityId()))) {
            log.debug("AccountForm.cityId is empty");
            errors.rejectValue("cityId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("stateId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getStateId()))) {
            log.debug("AccountForm.stateId is empty");
            errors.rejectValue("stateId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("countryId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCountryId()))) {
            errors.rejectValue("countryId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountForm.countryId is invalid");
            return;
        }
        if(!fieldsToEscape.contains("pincode") && StringUtils.hasText(StringUtils.trimWhitespace(form.getPincode()))) {
            errors.rejectValue("pincode", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountForm.pincode is invalid");
            return;
        }
        if(!fieldsToEscape.contains("accountId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getAccountId()))) {
            log.debug("AccountForm.accountId is empty");
            errors.rejectValue("accountId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("accountId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))){
            String accountId = form.getAccountId();
            try {
                AccountVo accountVo = accountService.retrieveDetailsById(accountId, Optional.of(TOABCascadeLevel.ONE));
                if(!accountVo.getActive()) {
                    log.debug("AccountForm.accountId is inactive");
                    errors.rejectValue("accountId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (AccountException e) {
                log.debug("AccountForm.accountId is invalid");
                errors.rejectValue("accountId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
                return;
            }
        }
    }

}
