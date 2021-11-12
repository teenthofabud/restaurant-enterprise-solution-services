package com.teenthofabud.restaurant.solution.customer.address.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressDto;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressEntity;
import com.teenthofabud.restaurant.solution.customer.account.repository.AccountRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class AddressDto2EntityConverter implements ComparativePatchConverter<AddressDto, AddressEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 8;

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
    public void compareAndMap(AddressDto dto, AddressEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optAccountId = dto.getAccountId();
        if(!fieldsToEscape.contains("accountId") && optAccountId.isPresent()) {
            Long accountId = Long.parseLong(optAccountId.get());
            Optional<AccountEntity> optAccountEntity = accountRepository.findById(accountId);
            actualEntity.setAccount(optAccountEntity.get());
            changeSW[i++] = true;
            log.debug("AccountDto.accountId is valid");
        }
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent()) {
            actualEntity.setName(optName.get());
            changeSW[i++] = true;
            log.debug("AccountDto.name is valid");
        }
        Optional<String> optAddressLine1 = dto.getAddressLine1();
        if(!fieldsToEscape.contains("addressLine1") && optAddressLine1.isPresent()) {
            actualEntity.setAddressLine1(optAddressLine1.get());
            changeSW[i++] = true;
            log.debug("AccountDto.addressLine1 is valid");
        }
        Optional<String> optAddressLine2 = dto.getAddressLine2();
        if(!fieldsToEscape.contains("addressLine2") && optAddressLine2.isPresent()) {
            actualEntity.setAddressLine2(optAddressLine2.get());
            changeSW[i++] = true;
            log.debug("AccountDto.addressLine2 is valid");
        }
        Optional<String> optCountryId = dto.getCountryId();
        if(!fieldsToEscape.contains("countryId") && optCountryId.isPresent()) {
            actualEntity.setCountryId(optCountryId.get());
            changeSW[i++] = true;
            log.debug("AccountDto.countryId is valid");
        }
        Optional<String> optPincode = dto.getPincode();
        if(!fieldsToEscape.contains("pincode") && optPincode.isPresent()) {
            actualEntity.setPincode(optPincode.get());
            changeSW[i++] = true;
            log.debug("AccountDto.pincode is valid");
        }
        Optional<String> optCityId = dto.getCityId();
        if(!fieldsToEscape.contains("cityId") && optCityId.isPresent()) {
            actualEntity.setCityId(optCityId.get());
            changeSW[i++] = true;
            log.debug("AccountDto.cityId is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("AccountDto.active is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided AccountDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided AccountDto attributes are valid");
    }

}
