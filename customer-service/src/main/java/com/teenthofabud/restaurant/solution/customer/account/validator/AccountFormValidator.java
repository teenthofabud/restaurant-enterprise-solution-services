package com.teenthofabud.restaurant.solution.customer.account.validator;

import com.teenthofabud.restaurant.solution.customer.account.data.AccountForm;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.validator.GenderIdValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.List;

@Component
@Slf4j
public class AccountFormValidator implements Validator {

    private String dobFormat;
    private List<String> fieldsToEscape;

    @Value("#{'${res.customer.account.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Value("${res.customer.dob.format}")
    public void setDobFormat(String dobFormat) {
        this.dobFormat = dobFormat;
    }

    private GenderIdValidator genderIdValidator;

    @Autowired
    public void setGenderIdValidator(GenderIdValidator genderIdValidator) {
        this.genderIdValidator = genderIdValidator;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(AccountForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        AccountForm form = (AccountForm) target;
        if(!fieldsToEscape.contains("firstName") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getFirstName()))) {
            log.debug("AccountForm.firstName is empty");
            errors.rejectValue("firstName", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("lastName") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getLastName()))) {
            log.debug("AccountForm.lastName is empty");
            errors.rejectValue("lastName", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("countryCode") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCountryCode()))) {
            log.debug("AccountForm.countryCode is empty");
            errors.rejectValue("countryCode", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("phoneNumber") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getPhoneNumber()))) {
            log.debug("AccountForm.phoneNumber is empty");
            errors.rejectValue("phoneNumber", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("emailId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getEmailId()))) {
            log.debug("AccountForm.emailId is empty");
            errors.rejectValue("emailId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        }
        if(!fieldsToEscape.contains("dateOfBirth") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getDateOfBirth()))) {
            errors.rejectValue("dateOfBirth", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountForm.dateOfBirth is invalid");
            return;
        } else if(!fieldsToEscape.contains("dateOfBirth") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDateOfBirth()))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(dobFormat);
                LocalDate.parse(form.getDateOfBirth(), dtf);
            } catch (DateTimeParseException e) {
                errors.rejectValue("dateOfBirth", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
                log.debug("AccountForm.dateOfBirth is invalid");
                return;
            }
        }
        if(!fieldsToEscape.contains("genderId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getGenderId()))) {
            log.debug("AccountForm.genderId is empty");
            errors.rejectValue("genderId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("genderId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getGenderId()))){
            Errors err = new DirectFieldBindingResult(form.getGenderId(), "AccountForm");
            genderIdValidator.validate(form.getGenderId(), err);
            if(err.hasErrors()) {
                log.debug("AccountForm.genderId is invalid");
                CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AccountForm error detail: {}", ec);
                errors.rejectValue("genderId", ec.name());
                return;
            }
        }
    }

}
