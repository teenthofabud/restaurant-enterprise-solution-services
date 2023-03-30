package com.teenthofabud.restaurant.solution.engagement.utils;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.CheckInEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.ReservationEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.WalkInEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.factory.CheckInBeanFactory;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.CheckInService;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.converter.TableAllocationEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationEntity;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationVo;
import lombok.extern.slf4j.Slf4j;
import org.reflections.ReflectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

import java.lang.reflect.Field;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@Slf4j
@Component
public class EngagementServiceHelper<T extends CheckInForm, U extends CheckInVo, V extends CheckInEntity> {

    private CheckInBeanFactory checkInBeanFactory;

    private TableAllocationEntity2VoConverter tableAllocationEntity2VoConverter;

    @Autowired
    public void setTableAllocationEntity2VoConverter(TableAllocationEntity2VoConverter tableAllocationEntity2VoConverter) {
        this.tableAllocationEntity2VoConverter = tableAllocationEntity2VoConverter;
    }

    @Autowired
    public void setCheckInBeanFactory(CheckInBeanFactory checkInBeanFactory) {
        this.checkInBeanFactory = checkInBeanFactory;
    }

    /*public List<CheckInVo> checkInEntity2DetailedVo(List<? extends CheckInEntity> checkInEntityList) {
        List<CheckInVo> checkInDetailsList = new LinkedList<>();
        if(checkInEntityList != null && !checkInEntityList.isEmpty()) {
            for(CheckInEntity entity : checkInEntityList) {
                CheckInVo vo = this.checkInEntity2DetailedVo(entity);
                checkInDetailsList.add(vo);
            }
        }
        return checkInDetailsList;
    }*/

    public Optional<? extends CheckInVo> checkInEntity2DetailedVo(CheckInEntity checkInEntity) {
        Optional<? extends CheckInEntity2VoConverter> optionalCheckInEntity2VoConverter = this.checkInBeanFactory.getCheckInEntity2VoConverter(checkInEntity.getType().name());
        CheckInEntity2VoConverter checkInEntity2VoConverter = optionalCheckInEntity2VoConverter.get();
        if(checkInEntity != null) {
            Optional<? extends CheckInVo> optionalCheckInVo = Optional.of(checkInEntity2VoConverter.convert(checkInEntity));
            log.debug("Converting {} to {}", checkInEntity, optionalCheckInVo.get());
            return optionalCheckInVo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "checkIn entity is null" });
    }

    public WalkInVo walkInEntity2DetailedVo(WalkInEntity walkInEntity) {
        Optional<? extends CheckInEntity2VoConverter> optionalCheckInEntity2VoConverter = this.checkInBeanFactory.getCheckInEntity2VoConverter(CheckInType.WALK_IN.name());
        WalkInEntity2VoConverter walkInEntity2VoConverter = (WalkInEntity2VoConverter) optionalCheckInEntity2VoConverter.get();
        if(walkInEntity != null) {
            WalkInVo vo = walkInEntity2VoConverter.convert(walkInEntity);
            log.debug("Converting {} to {}", walkInEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "walkIn entity is null" });
    }

    public List<WalkInVo> walkInEntity2DetailedVo(List<WalkInEntity> walkInEntityList) {
        List<WalkInVo> walkInDetailsList = new LinkedList<>();
        if(walkInEntityList != null && !walkInEntityList.isEmpty()) {
            for(WalkInEntity entity : walkInEntityList) {
                WalkInVo vo = this.walkInEntity2DetailedVo(entity);
                walkInDetailsList.add(vo);
            }
        }
        return walkInDetailsList;
    }

    public ReservationVo reservationEntity2DetailedVo(ReservationEntity reservationEntity) {
        Optional<? extends CheckInEntity2VoConverter> optionalCheckInEntity2VoConverter = this.checkInBeanFactory.getCheckInEntity2VoConverter(CheckInType.RESERVATION.name());
        ReservationEntity2VoConverter reservationEntity2VoConverter = (ReservationEntity2VoConverter) optionalCheckInEntity2VoConverter.get();
        if(reservationEntity != null) {
            ReservationVo vo = reservationEntity2VoConverter.convert(reservationEntity);
            log.debug("Converting {} to {}", reservationEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "reservation entity is null" });
    }

    public List<ReservationVo> reservationEntity2DetailedVo(List<ReservationEntity> reservationEntityList) {
        List<ReservationVo> reservationDetailsList = new LinkedList<>();
        if(reservationEntityList != null && !reservationEntityList.isEmpty()) {
            for(ReservationEntity entity : reservationEntityList) {
                ReservationVo vo = this.reservationEntity2DetailedVo(entity);
                reservationDetailsList.add(vo);
            }
        }
        return reservationDetailsList;
    }

    public List<TableAllocationVo> tableAllocationEntity2DetailedVo(List<TableAllocationEntity> tableAllocationEntityList) {
        List<TableAllocationVo> tableAllocationVoList = new LinkedList<>();
        if(tableAllocationEntityList != null && !tableAllocationEntityList.isEmpty()) {
            for(TableAllocationEntity entity : tableAllocationEntityList) {
                TableAllocationVo vo = this.tableAllocationEntity2DetailedVo(entity);
                tableAllocationVoList.add(vo);
            }
        }
        return tableAllocationVoList;
    }

    public TableAllocationVo tableAllocationEntity2DetailedVo(TableAllocationEntity tableAllocationEntity) {
        if(tableAllocationEntity != null) {
            TableAllocationVo vo = tableAllocationEntity2VoConverter.convert(tableAllocationEntity);
            log.debug("Converting {} to {}", tableAllocationEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "tableAllocationEntity entity is null" });
    }
}
