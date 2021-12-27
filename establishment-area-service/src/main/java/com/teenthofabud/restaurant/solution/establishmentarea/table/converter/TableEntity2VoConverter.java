package com.teenthofabud.restaurant.solution.establishmentarea.table.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableVo;
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
public class TableEntity2VoConverter extends TOABBaseEntity2VoConverter<TableEntity, TableVo> implements Converter<TableEntity, TableVo> {

    private List<String> fieldsToEscape;
    private EstablishmentAreaServiceHelper establishmentAreaServiceHelper;

    @Autowired
    public void setEstablishmentAreaServiceHelper(EstablishmentAreaServiceHelper establishmentAreaServiceHelper) {
        this.establishmentAreaServiceHelper = establishmentAreaServiceHelper;
    }

    @Value("{'${res.establishment.area.table.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public TableVo convert(TableEntity entity) {
        TableVo vo = new TableVo();
        if(!fieldsToEscape.contains("tableId")) {
            vo.setTableId(entity.getTableId().toString());
        }
        if(!fieldsToEscape.contains("tableName")) {
            vo.setTableName(entity.getTableName());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        if(!fieldsToEscape.contains("capacity")) {
            vo.setDescription(entity.getCapacity());
        }
        if(!fieldsToEscape.contains("floorId")) {
            if(!fieldsToEscape.contains("floorId")) {
                Callable<FloorVo> floorEntity2VoConversion = () -> {
                    TOABRequestContextHolder.setCascadeLevelContext(TOABCascadeLevel.ZERO);
                    FloorVo floorVo = establishmentAreaServiceHelper.floorEntity2DetailedVo(entity.getFloor());
                    TOABRequestContextHolder.clearCascadeLevelContext();
                    return floorVo;
                };
                ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory("floorEntity2VoConversion-"));
                Future<FloorVo> floorEntity2VoConversionResult = executorService.submit(floorEntity2VoConversion);
                try {
                    FloorVo floorVo = floorEntity2VoConversionResult.get();
                    vo.setFloorVo(floorVo);
                    log.debug("Retrieved {} for floorId: {}", floorVo, entity.getFloor().getFlrId());
                } catch (InterruptedException | ExecutionException e) {
                    log.error("Unable to perform floorEntity2VoConversion", e);
                    throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "Unable to perform floorEntity2VoConversion",
                            new Object[] { "floorEntity2VoConversion failure: " + e.getMessage() });
                }
            }
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }
}
