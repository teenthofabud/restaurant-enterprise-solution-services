package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
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

    private List<String> fieldsToEscape;
    //private EstablishmentAreaServiceClient establishmentAreaServiceClient;
    private CustomerServiceClient customerServiceClient;

    @Value("#{'${res.engagement.checkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    /*@Autowired
    public void setEstablishmentAreaServiceClient(EstablishmentAreaServiceClient establishmentAreaServiceClient) {
        this.establishmentAreaServiceClient = establishmentAreaServiceClient;
    }*/

    @Autowired
    public void setCustomerServiceClient(CustomerServiceClient customerServiceClient) {
        this.customerServiceClient = customerServiceClient;
    }

    public U convert(T entity) {
        CheckInVo vo = new CheckInVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        /*if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("emailId")) {
            vo.setEmailId(entity.getEmailId());
        }
        if(!fieldsToEscape.contains("phoneNumber")) {
            vo.setPhoneNumber(entity.getPhoneNumber());
        }*/
        if(!fieldsToEscape.contains("notes")) {
            vo.setNotes(entity.getNotes());
        }
        /*if(!fieldsToEscape.contains("status")) {
            vo.setStatus(entity.getStatus().get().name());
        }*/
        if(!fieldsToEscape.contains("noOfPersons")) {
            vo.setNoOfPersons(entity.getNoOfPersons());
        }
        if(!fieldsToEscape.contains("sequence")) {
            vo.setSequence(entity.getSequence());
        }
        /*if(!fieldsToEscape.contains("tableId")) {
            this.expandSecondLevelFields(entity, vo, "tableId");
        }*/
        if(!fieldsToEscape.contains("accountId")) {
            this.expandSecondLevelFields(entity, vo, "accountId");
        }
        U child = this.convertChild((T) entity, (U) vo);
        super.expandAuditFields(entity, child);
        log.debug("Converted {} to {} ", entity, child);
        return child;
    }

    private void expandSecondLevelFields(CheckInEntity entity, CheckInVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("accountId") && fieldName.compareTo("accountId") == 0) {
                    Callable<AccountVo> accountEntity2VoConversion = () -> {
                        AccountVo accountVo = customerServiceClient.getAccountDetailsById(entity.getAccountId());
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
                /*if(!fieldsToEscape.contains("tableId") && fieldName.compareTo("tableId") == 0) {
                    Callable<TableVo> tableEntity2VoConversion = () -> {
                        TableVo tableVo = establishmentAreaServiceClient.getTableDetailsById(entity.getTableId());
                        return tableVo;
                    };
                    String tName = "tableEntity2VoConversion";
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory(tName + "-"));
                    Future<TableVo> tableEntity2VoConversionResult = executorService.submit(tableEntity2VoConversion);
                    try {
                        TableVo tableVo = tableEntity2VoConversionResult.get();
                        vo.setTable(tableVo);
                        log.debug("Retrieved {} for tableId: {}", tableVo, entity.getTableId());
                    } catch (InterruptedException | ExecutionException e) {
                        String msg = "Unable to perform " + tName;
                        log.error(msg, e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { tName + " failure: " + e.getMessage() });
                    }
                }*/
                break;
            default:
                //vo.setTableId(entity.getTableId());
                vo.setAccountId(entity.getAccountId());
                //log.debug("only first level cascaded for booking over tableId");
                log.debug("only first level cascaded for booking over accountId");
                break;
        }
    }

    protected abstract U convertChild(T checkInEntityChild, CheckInVo checkInVo);
}
