package com.teenthofabud.restaurant.solution.customer.account.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class AccountEntitySelfMapper implements SingleChannelMapper<AccountEntity> {

    @Override
    public Optional<AccountEntity> compareAndMap(AccountEntity source, AccountEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source AccountEntity.id is valid");
        }
        if(source.getGenderId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getGenderId())) && source.getGenderId().compareTo(target.getGenderId()) != 0) {
            target.setGenderId(source.getGenderId());
            changeSW = true;
            log.debug("Source AccountEntity.genderId is valid");
        }
        if(source.getDateOfBirth() != null && source.getDateOfBirth().compareTo(target.getDateOfBirth()) != 0) {
            target.setDateOfBirth(source.getDateOfBirth());
            changeSW = true;
            log.debug("Source AccountEntity.dateOfBirth is valid");
        }
        if(source.getFirstName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getFirstName())) && source.getFirstName().compareTo(target.getFirstName()) != 0) {
            target.setFirstName(source.getFirstName());
            changeSW = true;
            log.debug("Source AccountEntity.firstName is valid");
        }
        if(source.getLastName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getLastName())) && source.getLastName().compareTo(target.getLastName()) != 0) {
            target.setLastName(source.getLastName());
            changeSW = true;
            log.debug("Source AccountEntity.lastName is valid");
        }
        if(source.getCountryCode() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getCountryCode())) && source.getCountryCode().compareTo(target.getCountryCode()) != 0) {
            target.setCountryCode(source.getCountryCode());
            changeSW = true;
            log.debug("Source AccountEntity.countryCode is valid");
        }
        if(source.getPhoneNumber() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getPhoneNumber())) && source.getPhoneNumber().compareTo(target.getPhoneNumber()) != 0) {
            target.setPhoneNumber(source.getPhoneNumber());
            changeSW = true;
            log.debug("Source AccountEntity.phoneNumber is valid");
        }
        if(source.getEmailId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getEmailId())) && source.getEmailId().compareTo(target.getEmailId()) != 0) {
            target.setEmailId(source.getEmailId());
            changeSW = true;
            log.debug("Source AccountEntity.emailId is valid");
        }
        if(changeSW) {
            log.debug("All provided AccountEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided AccountEntity attributes are valid");
            return Optional.empty();
        }
    }
}
