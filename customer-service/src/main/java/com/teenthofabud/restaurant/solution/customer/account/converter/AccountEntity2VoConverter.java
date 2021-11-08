package com.teenthofabud.restaurant.solution.customer.account.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.data.GenderVo;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.proxy.GenderServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class AccountEntity2VoConverter extends TOABBaseEntity2VoConverter<AccountEntity, AccountVo> implements Converter<AccountEntity, AccountVo> {

    private GenderServiceClient genderServiceClient;

    private List<String> fieldsToEscape;

    @Value("#{'${res.customer.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setGenderServiceClient(GenderServiceClient genderServiceClient) {
        this.genderServiceClient = genderServiceClient;
    }

    @Override
    public AccountVo convert(AccountEntity entity) {
        AccountVo vo = new AccountVo();
        vo.setFirstName(entity.getFirstName());
        vo.setLastName(entity.getLastName());
        if(!fieldsToEscape.contains("dateOfBirth")) {
            vo.setDateOfBirth(entity.getDateOfBirth());
        }
        if(!fieldsToEscape.contains("genderId")) {
            this.expandSecondLevelFields(entity, vo);
        }
        super.expandAuditFields(entity, vo);
        vo.setId(entity.getId().toString());
        vo.setActive(entity.getActive());
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

    private void expandSecondLevelFields(AccountEntity entity, AccountVo vo) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                GenderVo genderVo = genderServiceClient.getGenderDetailsById(entity.getGenderId().toString());
                vo.setGender(genderVo);
                log.debug("Retrieved {} for genderId: {}", vo, entity.getGenderId());
                break;
            default:
                log.debug("only second level cascading is allowed in target type");
                break;
        }
    }

}
