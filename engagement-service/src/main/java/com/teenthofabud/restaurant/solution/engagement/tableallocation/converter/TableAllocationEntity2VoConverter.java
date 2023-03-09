package com.teenthofabud.restaurant.solution.engagement.tableallocation.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInVo;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.data.TableVo;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.proxy.EstablishmentAreaServiceClient;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationEntity;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationVo;
import com.teenthofabud.restaurant.solution.engagement.utils.EngagementServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.concurrent.CustomizableThreadFactory;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.*;


@Component
@Slf4j
public class TableAllocationEntity2VoConverter extends TOABBaseEntity2VoConverter<TableAllocationEntity, TableAllocationVo> {

    private EstablishmentAreaServiceClient establishmentAreaServiceClient;

    private EngagementServiceHelper engagementServiceHelper;

    private List<String> fieldsToEscape;

    @Value("#{'${res.engagement.tableAllocation.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setEstablishmentAreaServiceClient(EstablishmentAreaServiceClient establishmentAreaServiceClient) {
        this.establishmentAreaServiceClient = establishmentAreaServiceClient;
    }

    @Autowired
    public void setEngagementServiceHelper(EngagementServiceHelper engagementServiceHelper) {
        this.engagementServiceHelper = engagementServiceHelper;
    }

    public TableAllocationVo convert(TableAllocationEntity entity) {
        TableAllocationVo vo = new TableAllocationVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("notes")) {
            vo.setNotes(entity.getNotes());
        }
        /*if(!fieldsToEscape.contains("status")) {
            vo.setStatus(entity.getStatus().get().name());
        }*/
        if(!fieldsToEscape.contains("tableId")) {
            this.expandSecondLevelFields(entity, vo, "tableId");
        }
        if(!fieldsToEscape.contains("checkInId")) {
            expandSecondLevelFields(entity, vo, "checkInId");
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

    private void expandSecondLevelFields(TableAllocationEntity entity, TableAllocationVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("tableId") && fieldName.compareTo("tableId") == 0) {
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
                        e.printStackTrace();
                        String msg = "Unable to perform " + tName;
                        log.error(msg, e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { tName + " failure: " + e.getMessage() });
                    }
                }
                if(!fieldsToEscape.contains("checkInId") && fieldName.compareTo("checkInId") == 0) {
                    Callable<CheckInVo> checkInEntity2VoConversion = () -> {
                        TOABRequestContextHolder.setCascadeLevelContext(TOABCascadeLevel.ZERO);
                        Optional<? extends CheckInVo> optionalCheckInVo = engagementServiceHelper.checkInEntity2DetailedVo(entity.getCheckIn());
                        TOABRequestContextHolder.clearCascadeLevelContext();
                        return optionalCheckInVo.get();
                    };
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory("checkInEntity2VoConversion-"));
                    Future<CheckInVo> checkInEntity2VoConversionResult = executorService.submit(checkInEntity2VoConversion);
                    try {
                        CheckInVo CheckInVo = checkInEntity2VoConversionResult.get();
                        vo.setCheckIn(CheckInVo);
                        log.debug("Retrieved {} for CheckInId: {}", CheckInVo, entity.getCheckIn().getId());
                    } catch (InterruptedException | ExecutionException e) {
                        log.error("Unable to perform checkInEntity2VoConversion", e);
                        e.printStackTrace();
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "Unable to perform checkInEntity2VoConversion",
                                new Object[] { "checkInEntity2VoConversion failure: " + e.getMessage() });
                    }
                }
                break;
            default:
                vo.setTableId(entity.getTableId());
                vo.setCheckInId(entity.getCheckIn().getId().toString());
                log.debug("only first level cascaded for booking over tableId");
                break;
        }
    }

}
