package com.teenthofabud.restaurant.solution.customer.account.validator;

import com.teenthofabud.restaurant.solution.customer.account.data.AccountDto;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.validator.GenderIdValidator;
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
import java.util.Optional;

@Component
@Slf4j
public class AccountDtoValidator implements Validator {

    private String dobFormat;
    private List<String> fieldsToEscape;

    @Value("#{'${res.customer.fields-to-escape}'.split(',')}")
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
        return clazz.isAssignableFrom(AccountDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        AccountDto dto = (AccountDto) target;
        Optional<String> optFirstName = dto.getFirstName();
        if(!fieldsToEscape.contains("firstName") && optFirstName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optFirstName.get()))) {
            errors.rejectValue("firstName", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountDto.firstName is invalid");
            return;
        }
        Optional<String> optLastName = dto.getLastName();
        if(!fieldsToEscape.contains("lastName") && optLastName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optLastName.get()))) {
            errors.rejectValue("lastName", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountDto.lastName is invalid");
            return;
        }
        Optional<String> optCountryCode = dto.getCountryCode();
        if(!fieldsToEscape.contains("countryCode") && optCountryCode.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optCountryCode.get()))) {
            errors.rejectValue("countryCode", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountDto.countryCode is invalid");
            return;
        }
        Optional<String> optPhoneNumber = dto.getPhoneNumber();
        if(!fieldsToEscape.contains("phoneNumber") && optPhoneNumber.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optPhoneNumber.get()))) {
            errors.rejectValue("phoneNumber", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountDto.phoneNumber is invalid");
            return;
        }
        Optional<String> optEmailId = dto.getEmailId();
        if(!fieldsToEscape.contains("emailId") && optEmailId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optEmailId.get()))) {
            errors.rejectValue("emailId", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
            log.debug("AccountDto.emailId is invalid");
            return;
        }
        Optional<String> optDateOfBirth = dto.getDateOfBirth();
        if(!fieldsToEscape.contains("dateOfBirth") && optDateOfBirth.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optDateOfBirth.get()))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(dobFormat);
                LocalDate.parse(optDateOfBirth.get(), dtf);
            } catch (DateTimeParseException e) {
                errors.rejectValue("dateOfBirth", CustomerErrorCode.CUST_ATTRIBUTE_INVALID.name());
                log.debug("AccountDto.dateOfBirth is invalid");
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
        Optional<String> optGenderId = dto.getGenderId();
        if(!fieldsToEscape.contains("genderId") && optGenderId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optGenderId.get()))) {
            String genderId = optGenderId.get();
            Errors err = new DirectFieldBindingResult(genderId, "AccountDto");
            genderIdValidator.validate(genderId, err);
            if(err.hasErrors()) {
                log.debug("AccountDto.genderId is invalid");
                CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("AccountDto error detail: {}", ec);
                errors.rejectValue("genderId", ec.name());
                return;
            }
        }
    }

}
