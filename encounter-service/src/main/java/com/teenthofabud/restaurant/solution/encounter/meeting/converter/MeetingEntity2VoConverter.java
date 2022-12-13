package com.teenthofabud.restaurant.solution.encounter.meeting.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.encounter.integration.customer.data.AccountVo;
import com.teenthofabud.restaurant.solution.encounter.integration.customer.proxy.CustomerServiceClient;
import com.teenthofabud.restaurant.solution.encounter.meeting.constants.MeetingType;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.concurrent.CustomizableThreadFactory;

import java.util.List;
import java.util.concurrent.*;

@Slf4j
public abstract class MeetingEntity2VoConverter<T extends MeetingEntity, U extends MeetingVo>
        extends TOABBaseEntity2VoConverter<MeetingEntity, MeetingVo> {

    private List<String> fieldsToEscape;
    private CustomerServiceClient customerServiceClient;

    @Value("#{'${res.engagement.checkIn.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setCustomerServiceClient(CustomerServiceClient customerServiceClient) {
        this.customerServiceClient = customerServiceClient;
    }

    public U convert(T entity) {
        MeetingVo vo = new MeetingVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("sequence")) {
            vo.setSequence(entity.getSequence());
        }
        if(!fieldsToEscape.contains("accountId")) {
            this.expandSecondLevelFields(entity, vo, "accountId");
        }
        TOABRequestContextHolder.setCascadeLevelContext(TOABCascadeLevel.TWO);
        super.expandAuditFields(entity, vo);
        TOABRequestContextHolder.clearCascadeLevelContext();
        U child = this.convertChild((T) entity, (U) vo);
        super.expandAuditFields(entity, child);
        log.debug("Converted {} to {} ", entity, child);
        return child;
    }

    private void expandSecondLevelFields(MeetingEntity entity, MeetingVo vo, String fieldName) {
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
                        vo.setAccountVo(accountVo);
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
                log.debug("only first level cascaded for booking over accountId");
                break;
        }
    }

    protected abstract MeetingType getContextualMeetingType();

    protected abstract U convertChild(T meetingEntityChild, MeetingVo meetingVo);

    public abstract List<String> getFieldsToEscape();
}
