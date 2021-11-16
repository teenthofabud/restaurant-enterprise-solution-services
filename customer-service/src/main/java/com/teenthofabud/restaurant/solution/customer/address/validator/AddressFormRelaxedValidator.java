package com.teenthofabud.restaurant.solution.customer.address.validator;

import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressForm;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;

import java.util.List;

@Component
@Slf4j
public class AddressFormRelaxedValidator implements RelaxedValidator<AddressForm>  {

    private List<String> fieldsToEscape;

    @Value("#{'${res.customer.address.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
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
        }
        log.debug("AddressForm.stateId is valid");
        if(!fieldsToEscape.contains("cityId") && form.getCityId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCityId()))) {
            errors.rejectValue("cityId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AddressForm.cityId is empty");
            return false;
        }
        log.debug("AddressForm.stateId is valid");
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
