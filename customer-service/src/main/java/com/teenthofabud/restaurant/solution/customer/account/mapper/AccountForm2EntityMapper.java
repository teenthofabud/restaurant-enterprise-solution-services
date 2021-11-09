package com.teenthofabud.restaurant.solution.customer.account.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class AccountForm2EntityMapper implements DualChannelMapper<AccountEntity, AccountForm> {

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

    @Override
    public Optional<AccountEntity> compareAndMap(AccountEntity actualEntity, AccountForm form) {
        AccountEntity expectedEntity = new AccountEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying AccountEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying AccountEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying AccountEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("firstName") && StringUtils.hasText(StringUtils.trimWhitespace(form.getFirstName()))
                && form.getFirstName().compareTo(actualEntity.getFirstName()) != 0) {
            expectedEntity.setFirstName(form.getFirstName());
            changeSW = true;
            log.debug("AccountForm.firstName: {} is different as AccountEntity.firstName: {}", form.getFirstName(), actualEntity.getFirstName());
        } else {
            expectedEntity.setFirstName(actualEntity.getFirstName());
            log.debug("AccountForm.firstName: is unchanged");
        }
        if(!fieldsToEscape.contains("lastName") && StringUtils.hasText(StringUtils.trimWhitespace(form.getLastName()))
                && form.getLastName().compareTo(actualEntity.getLastName()) != 0) {
            expectedEntity.setLastName(form.getLastName());
            changeSW = true;
            log.debug("AccountForm.lastName: {} is different as AccountEntity.lastName: {}", form.getLastName(), actualEntity.getLastName());
        } else {
            expectedEntity.setLastName(actualEntity.getLastName());
            log.debug("AccountForm.lastName: is unchanged");
        }
        if(!fieldsToEscape.contains("countryCode") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCountryCode()))
                && form.getCountryCode().compareTo(actualEntity.getCountryCode()) != 0) {
            expectedEntity.setCountryCode(form.getCountryCode());
            changeSW = true;
            log.debug("AccountForm.countryCode: {} is different as AccountEntity.countryCode: {}", form.getCountryCode(), actualEntity.getCountryCode());
        } else {
            expectedEntity.setCountryCode(actualEntity.getCountryCode());
            log.debug("AccountForm.countryCode: is unchanged");
        }
        if(!fieldsToEscape.contains("phoneNumber") && StringUtils.hasText(StringUtils.trimWhitespace(form.getPhoneNumber()))
                && form.getPhoneNumber().compareTo(actualEntity.getPhoneNumber()) != 0) {
            expectedEntity.setPhoneNumber(form.getPhoneNumber());
            changeSW = true;
            log.debug("AccountForm.phoneNumber: {} is different as AccountEntity.phoneNumber: {}", form.getPhoneNumber(), actualEntity.getPhoneNumber());
        } else {
            expectedEntity.setPhoneNumber(actualEntity.getPhoneNumber());
            log.debug("AccountForm.phoneNumber: is unchanged");
        }
        if(!fieldsToEscape.contains("emailId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getEmailId()))
                && form.getEmailId().compareTo(actualEntity.getEmailId()) != 0) {
            expectedEntity.setEmailId(form.getEmailId());
            changeSW = true;
            log.debug("AccountForm.emailId: {} is different as AccountEntity.emailId: {}", form.getEmailId(), actualEntity.getEmailId());
        } else {
            expectedEntity.setEmailId(actualEntity.getEmailId());
            log.debug("AccountForm.emailId: is unchanged");
        }
        if(!fieldsToEscape.contains("genderId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getGenderId()))
                && form.getGenderId().compareTo(actualEntity.getGenderId().toString()) != 0) {
            expectedEntity.setGenderId(form.getGenderId());
            changeSW = true;
            log.debug("AccountForm.genderId: {} is different as AccountEntity.genderId: {}", form.getGenderId(), actualEntity.getGenderId());
        } else {
            expectedEntity.setGenderId(actualEntity.getGenderId());
            log.debug("AccountForm.genderId: is unchanged");
        }
        if(!fieldsToEscape.contains("dateOfBirth") && form.getDateOfBirth() != null) {
            LocalDate localDate = LocalDate.parse(form.getDateOfBirth(), DateTimeFormatter.ofPattern(dobFormat));
            if(actualEntity.getDateOfBirth().compareTo(localDate) != 0) {
                expectedEntity.setDateOfBirth(localDate);
                changeSW = true;
                log.debug("AccountForm.dateOfBirth: {} is different as AccountEntity.dateOfBirth: {}", form.getDateOfBirth(), actualEntity.getDateOfBirth());
            } else {
                expectedEntity.setDateOfBirth(actualEntity.getDateOfBirth());
                log.debug("AccountForm.dateOfBirth: is unchanged");
            }
        } else {
            expectedEntity.setDateOfBirth(actualEntity.getDateOfBirth());
            log.debug("AccountForm.dateOfBirth: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
