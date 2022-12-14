package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInVo;
import com.teenthofabud.restaurant.solution.engagement.integration.customer.data.AccountVo;
import com.teenthofabud.restaurant.solution.engagement.integration.customer.proxy.CustomerServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.concurrent.CustomizableThreadFactory;

import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;


@Slf4j
public abstract class CheckInEntity2VoConverter<T extends CheckInEntity, U extends CheckInVo> extends TOABBaseEntity2VoConverter<CheckInEntity, CheckInVo> {

    private CustomerServiceClient customerServiceClient;

    public abstract List<String> getFieldsToEscape();

    @Autowired
    public void setCustomerServiceClient(CustomerServiceClient customerServiceClient) {
        this.customerServiceClient = customerServiceClient;
    }

    public U convert(T entity) {
        CheckInVo vo = new CheckInVo();
        if(!getFieldsToEscape().contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!getFieldsToEscape().contains("notes")) {
            vo.setNotes(entity.getNotes());
        }
        /*if(!getFieldsToEscape().contains("status")) {
            vo.setStatus(entity.getStatus().get().name());
        }*/
        if(!getFieldsToEscape().contains("noOfPersons")) {
            vo.setNoOfPersons(entity.getNoOfPersons());
        }
        if(!getFieldsToEscape().contains("sequence")) {
            vo.setSequence(entity.getSequence());
        }
        if(!getFieldsToEscape().contains("type")) {
            vo.setType(getContextualCheckInType().name());
        }

        if(!getFieldsToEscape().contains("accountId")) {
            this.expandSecondLevelFields(entity, vo, "accountId");
        }
        TOABRequestContextHolder.setCascadeLevelContext(TOABCascadeLevel.TWO);
        super.expandAuditFields(entity, vo);
        TOABRequestContextHolder.clearCascadeLevelContext();
        U child = this.convertChild((T) entity, (U) vo);
        log.debug("Converted {} to {} ", entity, child);
        return child;
    }

    protected abstract CheckInType getContextualCheckInType();

    private void expandSecondLevelFields(CheckInEntity entity, CheckInVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!getFieldsToEscape().contains("accountId") && fieldName.compareTo("accountId") == 0) {
                    Callable<AccountVo> accountEntity2VoConversion = () -> {
                        AccountVo accountVo = customerServiceClient.getAccountDetailsById(entity.getAccountId(), TOABCascadeLevel.TWO.getLevelCode());
                        return accountVo;
                    };
                    String tName = "accountEntity2VoConversion";
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory(tName + "-"));
                    Future<AccountVo> accountEntity2VoConversionResult = executorService.submit(accountEntity2VoConversion);
                    try {
                        AccountVo accountVo = accountEntity2VoConversionResult.get();
                        vo.setAccount(accountVo);
                        log.debug("Retrieved {} for accountId: {}", accountVo, entity.getAccountId());
                    } catch (InterruptedException | ExecutionException e) {
                        String msg = "Unable to perform " + tName;
                        log.error(msg, e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { tName + " failure: " + e.getMessage() });
                    }
                }
                break;
            default:
                vo.setAccountId(entity.getAccountId());
                //log.debug("only first level cascaded for booking over tableId");
                log.debug("only first level cascaded for booking over accountId");
                break;
        }
    }

    protected abstract U convertChild(T checkInEntityChild, CheckInVo checkInVo);
}
