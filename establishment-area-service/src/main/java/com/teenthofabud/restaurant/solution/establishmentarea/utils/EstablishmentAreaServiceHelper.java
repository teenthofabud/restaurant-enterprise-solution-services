package com.teenthofabud.restaurant.solution.establishmentarea.utils;

import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.converter.FloorEntity2VoConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.converter.KitchenEntity2VoConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenException;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenVo;
import com.teenthofabud.restaurant.solution.establishmentarea.table.converter.TableEntity2VoConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableException;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

@Slf4j
@Component
public class EstablishmentAreaServiceHelper {

    private FloorEntity2VoConverter floorEntity2VoConverter;
    private TableEntity2VoConverter tableEntity2VoConverter;
    private KitchenEntity2VoConverter kitchenEntity2VoConverter;

    @Autowired
    public void setFloorEntity2VoConverter(FloorEntity2VoConverter floorEntity2VoConverter) {
        this.floorEntity2VoConverter = floorEntity2VoConverter;
    }

    @Autowired
    public void setTableEntity2VoConverter(TableEntity2VoConverter tableEntity2VoConverter) {
        this.tableEntity2VoConverter = tableEntity2VoConverter;
    }

    @Autowired
    public void setKitchenEntity2VoConverter(KitchenEntity2VoConverter kitchenEntity2VoConverter) {
        this.kitchenEntity2VoConverter = kitchenEntity2VoConverter;
    }

    public List<FloorVo> floorEntity2DetailedVo(List<? extends FloorEntity> floorEntityList) throws FloorException {
        List<FloorVo> floorDetailsList = new LinkedList<>();
        if(floorEntityList != null && !floorEntityList.isEmpty()) {
            for(FloorEntity entity : floorEntityList) {
                FloorVo vo = this.floorEntity2DetailedVo(entity);
                log.debug("Converting {} to {}", entity, vo);
                floorDetailsList.add(vo);
            }
        }
        return floorDetailsList;
    }

    public FloorVo floorEntity2DetailedVo(FloorEntity floorEntity) throws FloorException {
        if(floorEntity != null) {
            FloorVo vo = floorEntity2VoConverter.convert(floorEntity);
            log.debug("Converting {} to {}", floorEntity, vo);
            return vo;
        }
        throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE, new Object[] { "floor entity is null" });
    }

    public List<TableVo> tableEntity2DetailedVo(List<? extends TableEntity> tableEntityList) {
        List<TableVo> tableDetailsList = new ArrayList<>(tableEntityList.size());
        for(TableEntity entity : tableEntityList) {
            TableVo vo = tableEntity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            tableDetailsList.add(vo);
        }
        return tableDetailsList;
    }

    public TableVo tableEntity2DetailedVo(TableEntity tableEntity) throws TableException {
        if(tableEntity != null) {
            TableVo vo = tableEntity2VoConverter.convert(tableEntity);
            log.debug("Converting {} to {}", tableEntity, vo);
            return vo;
        }
        throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE, new Object[] { "table entity is null" });
    }

    public List<KitchenVo> kitchenEntity2DetailedVo(List<? extends KitchenEntity> kitchenEntityList) {
        List<KitchenVo> kitchenDetailsList = new ArrayList<>(kitchenEntityList.size());
        for(KitchenEntity entity : kitchenEntityList) {
            KitchenVo vo = kitchenEntity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            kitchenDetailsList.add(vo);
        }
        return kitchenDetailsList;
    }

    public KitchenVo kitchenEntity2DetailedVo(KitchenEntity kitchenEntity) throws KitchenException {
        if(kitchenEntity != null) {
            KitchenVo vo = kitchenEntity2VoConverter.convert(kitchenEntity);
            log.debug("Converting {} to {}", kitchenEntity, vo);
            return vo;
        }
        throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE, new Object[] { "kitchen entity is null" });
    }

}
