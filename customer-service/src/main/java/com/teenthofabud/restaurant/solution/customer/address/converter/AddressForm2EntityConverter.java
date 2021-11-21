package com.teenthofabud.restaurant.solution.customer.address.converter;

import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressEntity;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressForm;
import com.teenthofabud.restaurant.solution.customer.account.repository.AccountRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class AddressForm2EntityConverter implements Converter<AddressForm, AddressEntity> {

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
    public AddressEntity convert(AddressForm form) {
        AddressEntity entity = new AddressEntity();
        if(!fieldsToEscape.contains("name")) {
            entity.setName(form.getName());
        }
        if(!fieldsToEscape.contains("addressLine1")) {
            entity.setAddressLine1(form.getAddressLine1());
        }
        if(!fieldsToEscape.contains("addressLine2")) {
            entity.setAddressLine2(form.getAddressLine2());
        }
        if(!fieldsToEscape.contains("accountId")) {
            Long accountId = Long.parseLong(form.getAccountId());
            Optional<AccountEntity> optAccountEntity = accountRepository.findById(accountId);
            entity.setAccount(optAccountEntity.get());
        }
        if(!fieldsToEscape.contains("stateId")) {
            entity.setStateId(form.getStateId());
        }
        if(!fieldsToEscape.contains("countryId")) {
            entity.setCountryId(form.getCountryId());
        }
        if(!fieldsToEscape.contains("cityId")) {
            entity.setCityId(form.getCityId());
        }
        if(!fieldsToEscape.contains("pincode")) {
            entity.setPincode(form.getPincode());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
