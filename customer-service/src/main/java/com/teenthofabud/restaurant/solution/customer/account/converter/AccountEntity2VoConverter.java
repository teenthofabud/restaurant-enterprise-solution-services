package com.teenthofabud.restaurant.solution.customer.account.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.proxy.MetadataServiceClient;
import com.teenthofabud.restaurant.solution.customer.utils.CustomerServiceHelper;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressEntity;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressVo;
import com.teenthofabud.restaurant.solution.customer.address.service.AddressService;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.data.GenderVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.scheduling.concurrent.CustomizableThreadFactory;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.concurrent.*;

@Component
@Slf4j
public class AccountEntity2VoConverter extends TOABBaseEntity2VoConverter<AccountEntity, AccountVo> implements Converter<AccountEntity, AccountVo> {

    private MetadataServiceClient metadataServiceClient;
    private AddressService addressService;
    private List<String> fieldsToEscape;
    private CustomerServiceHelper customerServiceHelper;

    @Value("#{'${res.customer.account.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setAccountServiceHelper(CustomerServiceHelper customerServiceHelper) {
        this.customerServiceHelper = customerServiceHelper;
    }

    @Autowired
    public void setAddressService(AddressService addressService) {
        this.addressService = addressService;
    }

    @Autowired
    public void setMetadataServiceClient(MetadataServiceClient metadataServiceClient) {
        this.metadataServiceClient = metadataServiceClient;
    }

    @Override
    public AccountVo convert(AccountEntity entity) {
        AccountVo vo = new AccountVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("firstName")) {
            vo.setFirstName(entity.getFirstName());
        }
        if(!fieldsToEscape.contains("lastName")) {
            vo.setLastName(entity.getLastName());
        }
        if(!fieldsToEscape.contains("countryCode")) {
            vo.setCountryCode(entity.getCountryCode());
        }
        if(!fieldsToEscape.contains("phoneNumber")) {
            vo.setPhoneNumber(entity.getPhoneNumber());
        }
        if(!fieldsToEscape.contains("dateOfBirth")) {
            vo.setDateOfBirth(entity.getDateOfBirth());
        }
        if(!fieldsToEscape.contains("genderId")) {
            this.expandSecondLevelFields(entity, vo, "genderId");
        }
        if(!fieldsToEscape.contains("addresses")) {
            this.expandSecondLevelFields(entity, vo, "addresses");
        }
        if(!fieldsToEscape.contains("emailId")) {
            vo.setEmailId(entity.getEmailId());
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

    @Deprecated
    private void expandSecondLevelFields(AccountEntity entity, AccountVo vo) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("genderId")) {
                    GenderVo genderVo = metadataServiceClient.getGenderDetailsById(entity.getGenderId().toString());
                    vo.setGender(genderVo);
                    log.debug("Retrieved {} for genderId: {}", vo, entity.getGenderId());
                }
                if(!fieldsToEscape.contains("addresses")) {
                    List<AddressEntity> addressEntities = entity.getAddresses();
                    List<AddressVo> addressDetailsList = customerServiceHelper.addressEntity2DetailedVo(addressEntities);
                    vo.setAddresses(addressDetailsList);
                    log.debug("Retrieved {} addresses for account id: {}", addressDetailsList.size(), entity.getId());
                }
                break;
            default:
                vo.setGenderId(entity.getGenderId().toString());
                log.debug("only first level cascaded for genderId");
                break;
        }
    }

    private void expandSecondLevelFields(AccountEntity entity, AccountVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("genderId") && fieldName.compareTo("genderId") == 0) {
                    GenderVo genderVo = metadataServiceClient.getGenderDetailsById(entity.getGenderId().toString());
                    vo.setGender(genderVo);
                    log.debug("Retrieved {} for genderId: {}", vo, entity.getGenderId());
                }
                if(!fieldsToEscape.contains("addresses") && fieldName.compareTo("addresses") == 0) {
                    Callable<List<AddressVo>> addressEntity2VoConversion = () -> {
                        TOABRequestContextHolder.setCascadeLevelContext(TOABCascadeLevel.ZERO);
                        List<AddressEntity> addressEntities = entity.getAddresses();
                        List<AddressVo> addressDetailsList = customerServiceHelper.addressEntity2DetailedVo(addressEntities);
                        TOABRequestContextHolder.clearCascadeLevelContext();
                        return addressDetailsList;
                    };
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory("addressEntity2VoConversion-"));
                    Future<List<AddressVo>> addressEntity2VoConversionResult = executorService.submit(addressEntity2VoConversion);
                    try {
                        List<AddressVo> addressDetailsList = addressEntity2VoConversionResult.get();
                        vo.setAddresses(addressDetailsList);
                        log.debug("Retrieved {} addresses for account id: {}", addressDetailsList.size(), entity.getId());
                    } catch (InterruptedException | ExecutionException e) {
                        log.error("Unable to perform addressEntity2VoConversion", e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "Unable to perform addressEntity2VoConversion",
                                new Object[] { "addressEntity2VoConversion failure: " + e.getMessage() });
                    }
                }
                break;
            default:
                vo.setGenderId(entity.getGenderId().toString());
                log.debug("only first level cascaded for genderId");
                break;
        }
    }

}
