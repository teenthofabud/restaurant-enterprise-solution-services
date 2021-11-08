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
        if(source.getGenderId() != null && source.getGenderId().compareTo(target.getGenderId()) != 0) {
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
        if(changeSW) {
            log.debug("All provided AccountEntity attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided AccountEntity attributes are valid");
            return Optional.empty();
        }
    }
}
