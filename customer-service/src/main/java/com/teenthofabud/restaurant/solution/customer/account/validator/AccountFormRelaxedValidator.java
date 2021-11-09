package com.teenthofabud.restaurant.solution.customer.account.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountForm;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;

@Component
@Slf4j
public class AccountFormRelaxedValidator implements RelaxedValidator<AccountForm>  {

    private List<String> fieldsToEscape;

    @Value("#{'${res.customer.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Boolean validateLoosely(AccountForm form, Errors errors) {
        if(!fieldsToEscape.contains("firstName") && form.getFirstName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getFirstName()))) {
            errors.rejectValue("firstName", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountForm.firstName is empty");
            return false;
        }
        log.debug("AccountForm.firstName is valid");
        if(!fieldsToEscape.contains("lastName") && form.getLastName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getLastName()))) {
            errors.rejectValue("lastName", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountForm.lastName is empty");
            return false;
        }
        log.debug("AccountForm.lastName is valid");
        if(!fieldsToEscape.contains("countryCode") && form.getCountryCode() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCountryCode()))) {
            errors.rejectValue("countryCode", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountForm.countryCode is empty");
            return false;
        }
        log.debug("AccountForm.countryCode is valid");
        if(!fieldsToEscape.contains("phoneNumber") && form.getPhoneNumber() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getPhoneNumber()))) {
            errors.rejectValue("phoneNumber", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountForm.phoneNumber is empty");
            return false;
        }
        log.debug("AccountForm.phoneNumber is valid");
        if(!fieldsToEscape.contains("emailId") && form.getEmailId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getEmailId()))) {
            errors.rejectValue("emailId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountForm.emailId is empty");
            return false;
        }
        log.debug("AccountForm.emailId is valid");
        if(!fieldsToEscape.contains("genderId") && form.getGenderId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getGenderId()))) {
            errors.rejectValue("genderId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountForm.genderId is empty");
            return false;
        }
        log.debug("AccountForm.genderId is valid");
        if(!fieldsToEscape.contains("dateOfBirth") && form.getDateOfBirth() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getGenderId()))) {
            errors.rejectValue("dateOfBirth", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountForm.dateOfBirth is empty");
            return false;
        }
        log.debug("AccountForm.dateOfBirth is valid");
        return true;
    }
}
