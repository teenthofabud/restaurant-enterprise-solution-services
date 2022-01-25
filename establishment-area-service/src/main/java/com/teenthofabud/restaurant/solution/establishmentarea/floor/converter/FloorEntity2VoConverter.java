package com.teenthofabud.restaurant.solution.establishmentarea.floor.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenVo;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableVo;
import com.teenthofabud.restaurant.solution.establishmentarea.utils.EstablishmentAreaServiceHelper;
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
public class FloorEntity2VoConverter extends TOABBaseEntity2VoConverter<FloorEntity, FloorVo> implements Converter<FloorEntity, FloorVo> {

    private List<String> fieldsToEscape;
    private EstablishmentAreaServiceHelper establishmentAreaServiceHelper;

    @Autowired
    public void setEstablishmentAreaServiceHelper(EstablishmentAreaServiceHelper establishmentAreaServiceHelper) {
        this.establishmentAreaServiceHelper = establishmentAreaServiceHelper;
    }

    @Value("{'${res.establishment.area.floor.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public FloorVo convert(FloorEntity entity) {
        FloorVo vo = new FloorVo();
        if(!fieldsToEscape.contains("flrId")) {
            vo.setFlrId(entity.getFlrId().toString());
        }
        if(!fieldsToEscape.contains("flrName")) {
            vo.setFlrName(entity.getFlrName());
        }
        if(!fieldsToEscape.contains("kitchens")) {
            this.expandSecondLevelFields(entity, vo, "kitchens");
        }
        if(!fieldsToEscape.contains("tables")) {
            this.expandSecondLevelFields(entity, vo, "tables");
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

    private void expandSecondLevelFields(FloorEntity entity, FloorVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("kitchens") && fieldName.compareTo("kitchens") == 0) {
                    Callable<List<KitchenVo>> kitchenEntity2VoConversion = () -> {
                        TOABRequestContextHolder.setCascadeLevelContext(TOABCascadeLevel.ZERO);
                        List<KitchenEntity> kitchenEntities = entity.getKitchen();
                        List<KitchenVo> kitchenDetailsList = establishmentAreaServiceHelper.kitchenEntity2DetailedVo(kitchenEntities);
                        TOABRequestContextHolder.clearCascadeLevelContext();
                        return kitchenDetailsList;
                    };
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory("kitchenEntity2VoConversion-"));
                    Future<List<KitchenVo>> kitchenEntity2VoConversionResult = executorService.submit(kitchenEntity2VoConversion);
                    try {
                        List<KitchenVo> kitchenDetailsList = kitchenEntity2VoConversionResult.get();
                        vo.setKitchens(kitchenDetailsList);
                        log.debug("Retrieved {} kitchens for floor id: {}", kitchenDetailsList.size(), entity.getFlrId());
                    } catch (InterruptedException | ExecutionException e) {
                        log.error("Unable to perform kitchenEntity2VoConversion", e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "Unable to perform kitchenEntity2VoConversion",
                                new Object[] { "kitchenEntity2VoConversion failure: " + e.getMessage() });
                    }
                }
                if(!fieldsToEscape.contains("tables") && fieldName.compareTo("tables") == 0) {
                    Callable<List<TableVo>> tableEntity2VoConversion = () -> {
                        TOABRequestContextHolder.setCascadeLevelContext(TOABCascadeLevel.ZERO);
                        List<TableEntity> tableEntities = entity.getTable();
                        List<TableVo> tableDetailsList = establishmentAreaServiceHelper.tableEntity2DetailedVo(tableEntities);
                        TOABRequestContextHolder.clearCascadeLevelContext();
                        return tableDetailsList;
                    };
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory("tableEntity2VoConversion-"));
                    Future<List<TableVo>> tableEntity2VoConversionResult = executorService.submit(tableEntity2VoConversion);
                    try {
                        List<TableVo> tableDetailsList = tableEntity2VoConversionResult.get();
                        vo.setTables(tableDetailsList);
                        log.debug("Retrieved {} tables for floor id: {}", tableDetailsList.size(), entity.getFlrId());
                    } catch (InterruptedException | ExecutionException e) {
                        log.error("Unable to perform tableEntity2VoConversion", e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "Unable to perform tableEntity2VoConversion",
                                new Object[] { "tableEntity2VoConversion failure: " + e.getMessage() });
                    }
                }
                break;
            default:
                break;
        }
    }
}
