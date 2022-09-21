package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseDocument2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInDocument;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInVo;
import com.teenthofabud.restaurant.solution.engagement.integration.customer.data.AccountVo;
import com.teenthofabud.restaurant.solution.engagement.integration.customer.proxy.CustomerServiceClient;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.data.TableVo;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.proxy.EstablishmentAreaServiceClient;
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
public class CheckInDocument2VoConverter extends TOABBaseDocument2VoConverter<CheckInDocument, CheckInVo> implements Converter<CheckInDocument, CheckInVo> {

    private List<String> fieldsToEscape;
    private EstablishmentAreaServiceClient establishmentAreaServiceClient;
    private CustomerServiceClient customerServiceClient;

    @Value("#{'${res.engagement.checkin.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setEstablishmentAreaServiceClient(EstablishmentAreaServiceClient establishmentAreaServiceClient) {
        this.establishmentAreaServiceClient = establishmentAreaServiceClient;
    }

    @Autowired
    public void setCustomerServiceClient(CustomerServiceClient customerServiceClient) {
        this.customerServiceClient = customerServiceClient;
    }

    @Override
    public CheckInVo convert(CheckInDocument document) {
        CheckInVo vo = new CheckInVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(document.getId());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(document.getName());
        }
        if(!fieldsToEscape.contains("emailId")) {
            vo.setEmailId(document.getEmailId());
        }
        if(!fieldsToEscape.contains("phoneNumber")) {
            vo.setPhoneNumber(document.getPhoneNumber());
        }
        if(!fieldsToEscape.contains("notes")) {
            vo.setNotes(document.getNotes());
        }
        if(!fieldsToEscape.contains("status")) {
            vo.setStatus(document.getStatus().get().name());
        }
        if(!fieldsToEscape.contains("noOfPersons")) {
            vo.setNoOfPersons(document.getNoOfPersons());
        }
        if(!fieldsToEscape.contains("sequence")) {
            vo.setSequence(document.getSequence());
        }
        if(!fieldsToEscape.contains("tableId")) {
            this.expandSecondLevelFields(document, vo, "tableId");
        }
        if(!fieldsToEscape.contains("accountId")) {
            this.expandSecondLevelFields(document, vo, "accountId");
        }
        super.expandAuditFields(document, vo);
        log.debug("Converted {} to {} ", document, vo);
        return vo;
    }

    private void expandSecondLevelFields(CheckInDocument document, CheckInVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("accountId") && fieldName.compareTo("accountId") == 0) {
                    Callable<AccountVo> accountDocument2VoConversion = () -> {
                        AccountVo accountVo = customerServiceClient.getAccountDetailsById(document.getAccountId());
                        return accountVo;
                    };
                    String tName = "accountDocument2VoConversion";
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory(tName + "-"));
                    Future<AccountVo> accountDocument2VoConversionResult = executorService.submit(accountDocument2VoConversion);
                    try {
                        AccountVo accountVo = accountDocument2VoConversionResult.get();
                        vo.setAccount(accountVo);
                        log.debug("Retrieved {} for accountId: {}", accountVo, document.getAccountId());
                    } catch (InterruptedException | ExecutionException e) {
                        String msg = "Unable to perform " + tName;
                        log.error(msg, e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { tName + " failure: " + e.getMessage() });
                    }
                }
                if(!fieldsToEscape.contains("tableId") && fieldName.compareTo("tableId") == 0) {
                    Callable<TableVo> tableDocument2VoConversion = () -> {
                        TableVo tableVo = establishmentAreaServiceClient.getTableDetailsById(document.getTableId());
                        return tableVo;
                    };
                    String tName = "tableDocument2VoConversion";
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory(tName + "-"));
                    Future<TableVo> tableDocument2VoConversionResult = executorService.submit(tableDocument2VoConversion);
                    try {
                        TableVo tableVo = tableDocument2VoConversionResult.get();
                        vo.setTable(tableVo);
                        log.debug("Retrieved {} for tableId: {}", tableVo, document.getTableId());
                    } catch (InterruptedException | ExecutionException e) {
                        String msg = "Unable to perform " + tName;
                        log.error(msg, e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { tName + " failure: " + e.getMessage() });
                    }
                }
                break;
            default:
                vo.setTableId(document.getTableId());
                vo.setAccountId(document.getAccountId());
                log.debug("only first level cascaded for booking over tableId and accountId");
                break;
        }
    }

}
