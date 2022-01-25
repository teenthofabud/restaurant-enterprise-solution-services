package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenVo;
import com.teenthofabud.restaurant.solution.establishmentarea.utils.EstablishmentAreaServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.scheduling.concurrent.CustomizableThreadFactory;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.*;

@Component
@Slf4j
public class KitchenEntity2VoConverter extends TOABBaseEntity2VoConverter<KitchenEntity, KitchenVo> implements Converter<KitchenEntity, KitchenVo> {

    private List<String> fieldsToEscape;

    private EstablishmentAreaServiceHelper establishmentAreaServiceHelper;

    @Autowired
    public void setEstablishmentAreaServiceHelper(EstablishmentAreaServiceHelper establishmentAreaServiceHelper) {
        this.establishmentAreaServiceHelper = establishmentAreaServiceHelper;
    }

    @Value("{'${res.establishment.area.kitchen.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public KitchenVo convert(KitchenEntity entity) {
        KitchenVo vo = new KitchenVo();
        if(!fieldsToEscape.contains("kitchenId")) {
            vo.setKitchenId(entity.getKitchenId().toString());
        }
        if(!fieldsToEscape.contains("kitchenName")) {
            vo.setKitchenName(entity.getKitchenName());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        if(!fieldsToEscape.contains("floorId")) {
            this.expandSecondLevelFields(entity, vo, "floorId");
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

    private void expandSecondLevelFields(KitchenEntity entity, KitchenVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("floorId") && fieldName.compareTo("floorId") == 0) {
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
                break;
            default:
                vo.setFloorId(entity.getFloor().getFlrId().toString());
                log.debug("only first level cascaded for floorId");
                break;
        }
    }
}
