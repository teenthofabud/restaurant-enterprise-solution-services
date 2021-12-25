package com.teenthofabud.restaurant.solution.customer.utils;

import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.customer.account.converter.AccountEntity2VoConverter;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountException;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.customer.address.converter.AddressEntity2VoConverter;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressEntity;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressException;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressVo;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

@Slf4j
@Component
public class CustomerServiceHelper {

    private AccountEntity2VoConverter accountEntity2VoConverter;
    private AddressEntity2VoConverter addressEntity2VoConverter;

    @Autowired
    public void setAccountEntity2VoConverter(AccountEntity2VoConverter accountEntity2VoConverter) {
        this.accountEntity2VoConverter = accountEntity2VoConverter;
    }

    @Autowired
    public void setAddressEntity2VoConverter(AddressEntity2VoConverter addressEntity2VoConverter) {
        this.addressEntity2VoConverter = addressEntity2VoConverter;
    }

    public List<AccountVo> accountEntity2DetailedVo(List<? extends AccountEntity> accountEntityList) {
        List<AccountVo> accountDetailsList = new LinkedList<>();
        if(accountEntityList != null && !accountEntityList.isEmpty()) {
            for(AccountEntity entity : accountEntityList) {
                AccountVo vo = this.accountEntity2DetailedVo(entity);
                accountDetailsList.add(vo);
            }
        }
        return accountDetailsList;
    }

    public AccountVo accountEntity2DetailedVo(AccountEntity accountEntity) {
        if(accountEntity != null) {
            AccountVo vo = accountEntity2VoConverter.convert(accountEntity);
            log.debug("Converting {} to {}", accountEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "account entity is null" });
    }

    public List<AddressVo> addressEntity2DetailedVo(List<? extends AddressEntity> addressEntityList) {
        List<AddressVo> addressDetailsList = new ArrayList<>(addressEntityList.size());
        for(AddressEntity entity : addressEntityList) {
            AddressVo vo = this.addressEntity2DetailedVo(entity);
            addressDetailsList.add(vo);
        }
        return addressDetailsList;
    }

    public AddressVo addressEntity2DetailedVo(AddressEntity addressEntity) {
        if(addressEntity != null) {
            AddressVo vo = addressEntity2VoConverter.convert(addressEntity);
            log.debug("Converting {} to {}", addressEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "address entity is null" });
    }

}
