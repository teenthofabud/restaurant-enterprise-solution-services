package com.teenthofabud.restaurant.solution.customer.address.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import com.teenthofabud.restaurant.solution.customer.account.repository.AccountRepository;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressEntity;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class AddressForm2EntityMapper implements DualChannelMapper<AddressEntity, AddressForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.customer.address.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private AccountRepository accountRepository;

    @Autowired
    public void setAccountRepository(AccountRepository accountRepository) {
        this.accountRepository = accountRepository;
    }

    @Override
    public Optional<AddressEntity> compareAndMap(AddressEntity actualEntity, AddressForm form) {
        AddressEntity expectedEntity = new AddressEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying AccountEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying AccountEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying AccountEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName()))
                && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("AccountForm.name: {} is different as AccountEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("AccountForm.name: is unchanged");
        }
        if(!fieldsToEscape.contains("addressLine1") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAddressLine1()))
                && form.getAddressLine1().compareTo(actualEntity.getAddressLine1()) != 0) {
            expectedEntity.setAddressLine1(form.getAddressLine1());
            changeSW = true;
            log.debug("AccountForm.addressLine1: {} is different as AccountEntity.addressLine1: {}", form.getAddressLine1(), actualEntity.getAddressLine1());
        } else {
            expectedEntity.setAddressLine1(actualEntity.getAddressLine1());
            log.debug("AccountForm.addressLine1: is unchanged");
        }
        if(!fieldsToEscape.contains("addressLine2") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAddressLine2()))
                && form.getAddressLine2().compareTo(actualEntity.getAddressLine2()) != 0) {
            expectedEntity.setAddressLine2(form.getAddressLine2());
            changeSW = true;
            log.debug("AccountForm.addressLine2: {} is different as AccountEntity.addressLine2: {}", form.getAddressLine2(), actualEntity.getAddressLine2());
        } else {
            expectedEntity.setAddressLine2(actualEntity.getAddressLine2());
            log.debug("AccountForm.addressLine2: is unchanged");
        }
        if(!fieldsToEscape.contains("countryId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCountryId()))
                && form.getCountryId().compareTo(actualEntity.getCountryId()) != 0) {
            expectedEntity.setCountryId(form.getCountryId());
            changeSW = true;
            log.debug("AccountForm.countryId: {} is different as AccountEntity.countryId: {}", form.getCountryId(), actualEntity.getCountryId());
        } else {
            expectedEntity.setCountryId(actualEntity.getCountryId());
            log.debug("AccountForm.countryId: is unchanged");
        }
        if(!fieldsToEscape.contains("pincode") && StringUtils.hasText(StringUtils.trimWhitespace(form.getPincode()))
                && form.getPincode().compareTo(actualEntity.getPincode()) != 0) {
            expectedEntity.setPincode(form.getPincode());
            changeSW = true;
            log.debug("AccountForm.pincode: {} is different as AccountEntity.pincode: {}", form.getPincode(), actualEntity.getPincode());
        } else {
            expectedEntity.setPincode(actualEntity.getPincode());
            log.debug("AccountForm.pincode: is unchanged");
        }
        if(!fieldsToEscape.contains("cityId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCityId()))
                && form.getCityId().compareTo(actualEntity.getCityId()) != 0) {
            expectedEntity.setCityId(form.getCityId());
            changeSW = true;
            log.debug("AccountForm.cityId: {} is different as AccountEntity.cityId: {}", form.getCityId(), actualEntity.getCityId());
        } else {
            expectedEntity.setCityId(actualEntity.getCityId());
            log.debug("AccountForm.cityId: is unchanged");
        }
        if(!fieldsToEscape.contains("stateId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getStateId()))
                && form.getStateId().compareTo(actualEntity.getStateId().toString()) != 0) {
            expectedEntity.setStateId(form.getStateId());
            changeSW = true;
            log.debug("AccountForm.stateId: {} is different as AccountEntity.stateId: {}", form.getStateId(), actualEntity.getStateId());
        } else {
            expectedEntity.setStateId(actualEntity.getStateId());
            log.debug("AccountForm.stateId: is unchanged");
        }
        if(!fieldsToEscape.contains("accountId") && form.getAccountId() != null) {
            Long accountId = Long.parseLong(form.getAccountId());
            Optional<AccountEntity> optAccountEntity = accountRepository.findById(accountId);
            if(actualEntity.getAccount().compareTo(optAccountEntity.get()) != 0) {
                expectedEntity.setAccount(optAccountEntity.get());
                changeSW = true;
                log.debug("AccountForm.accountId: {} is different as AccountEntity.accountId: {}", form.getAccountId(), actualEntity.getAccount().getId());
            } else {
                expectedEntity.setAccount(actualEntity.getAccount());
                log.debug("AccountForm.accountId: is unchanged");
            }
        } else {
            expectedEntity.setAccount(actualEntity.getAccount());
            log.debug("AccountForm.accountId: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
