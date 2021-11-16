package com.teenthofabud.restaurant.solution.customer.address.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.restaurant.solution.customer.account.converter.AccountEntity2VoConverter;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressEntity;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressVo;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.data.CityVo;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.data.CountryVo;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.data.StateVo;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.proxy.CountryStateCityApiClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class AddressEntity2VoConverter extends TOABBaseEntity2VoConverter<AddressEntity, AddressVo> implements Converter<AddressEntity, AddressVo> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.customer.account.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private AccountEntity2VoConverter accountEntity2VoConverter;
    private CountryStateCityApiClient countryStateCityApiClient;

    @Autowired
    public void setAccountEntity2VoConverter(AccountEntity2VoConverter accountEntity2VoConverter) {
        this.accountEntity2VoConverter = accountEntity2VoConverter;
    }

    @Autowired
    public void setCountryStateCityApiClient(CountryStateCityApiClient countryStateCityApiClient) {
        this.countryStateCityApiClient = countryStateCityApiClient;
    }

    @Override
    public AddressVo convert(AddressEntity entity) {
        AddressVo vo = new AddressVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("addressLine1")) {
            vo.setAddressLine1(entity.getAddressLine1());
        }
        if(!fieldsToEscape.contains("addressLine2")) {
            vo.setAddressLine2(entity.getAddressLine2());
        }
        if(!fieldsToEscape.contains("accountId") || !fieldsToEscape.contains("stateId") || !fieldsToEscape.contains("countryId") || !fieldsToEscape.contains("cityId")) {
            this.expandSecondLevelFields(entity, vo);
        }
        if(!fieldsToEscape.contains("pincode")) {
            vo.setPincode(entity.getPincode());
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

    private void expandSecondLevelFields(AddressEntity entity, AddressVo vo) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                AccountVo accountVo = accountEntity2VoConverter.convert(entity.getAccount());
                vo.setAccount(accountVo);
                log.debug("Retrieved {} for accountId: {}", accountVo, entity.getAccount().getId());
                CountryVo country = countryStateCityApiClient.getCountryDetailsFromISO2Code(entity.getCountryId());
                log.debug("Retrieved {} for country is: {}", country, entity.getCountryId());
                vo.setCountry(country);
                StateVo state = countryStateCityApiClient.getTheStateDetailsFromISO2Code(entity.getCountryId(), entity.getStateId());
                log.debug("Retrieved {} for state id: {}", state, entity.getStateId());
                vo.setState(state);
                List<CityVo> cities = countryStateCityApiClient.getTheListOfCitiesInACountry(entity.getCountryId());
                Optional<CityVo> optionalCity = cities.stream().filter(c -> c.getId().toString().compareTo(entity.getCityId()) == 0).findFirst();
                log.debug("Retrieved {} for city id: {}", optionalCity.get(), entity.getCityId());
                vo.setCity(optionalCity.get());
                break;
            default:
                vo.setAccountId(entity.getAccount().getId().toString());
                vo.setCityId(entity.getCityId());
                vo.setStateId(entity.getStateId());
                vo.setCountryId(entity.getCountryId());
                log.debug("only first level cascaded for accountId");
                break;
        }
    }

}
