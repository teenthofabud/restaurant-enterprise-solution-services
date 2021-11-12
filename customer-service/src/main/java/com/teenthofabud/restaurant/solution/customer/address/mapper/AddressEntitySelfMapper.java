package com.teenthofabud.restaurant.solution.customer.address.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class AddressEntitySelfMapper implements SingleChannelMapper<AddressEntity> {

    @Override
    public Optional<AddressEntity> compareAndMap(AddressEntity source, AddressEntity target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source AccountEntity.id is valid");
        }
        if(source.getName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getName())) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source AccountEntity.name is valid");
        }
        if(source.getAddressLine1() != null && source.getAddressLine1().compareTo(target.getAddressLine1()) != 0) {
            target.setAddressLine1(source.getAddressLine1());
            changeSW = true;
            log.debug("Source AccountEntity.addressLine1 is valid");
        }
        if(source.getAddressLine2() != null && source.getAddressLine2().compareTo(target.getAddressLine2()) != 0) {
            target.setAddressLine2(source.getAddressLine2());
            changeSW = true;
            log.debug("Source AccountEntity.addressLine2 is valid");
        }
        if(source.getCityId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getCityId())) && source.getCityId().compareTo(target.getCityId()) != 0) {
            target.setCityId(source.getCityId());
            changeSW = true;
            log.debug("Source AccountEntity.cityId is valid");
        }
        if(source.getCountryId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getCountryId())) && source.getCountryId().compareTo(target.getCountryId()) != 0) {
            target.setCountryId(source.getCountryId());
            changeSW = true;
            log.debug("Source AccountEntity.countryId is valid");
        }
        if(source.getPincode() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getPincode())) && source.getPincode().compareTo(target.getPincode()) != 0) {
            target.setPincode(source.getPincode());
            changeSW = true;
            log.debug("Source AccountEntity.pincode is valid");
        }
        if(source.getStateId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getStateId())) && source.getStateId().compareTo(target.getStateId()) != 0) {
            target.setStateId(source.getStateId());
            changeSW = true;
            log.debug("Source AccountEntity.stateId is valid");
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
