package com.teenthofabud.restaurant.solution.encounter.utils;

import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.encounter.delivery.converter.DeliveryEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryEntity;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryVo;
import com.teenthofabud.restaurant.solution.encounter.meeting.constants.MeetingType;
import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingVo;
import com.teenthofabud.restaurant.solution.encounter.meeting.factory.MeetingBeanFactory;
import com.teenthofabud.restaurant.solution.encounter.pickup.converter.PickUpEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpEntity;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

@Slf4j
@Component
public class EncounterServiceHelper<T extends MeetingForm, U extends MeetingVo, V extends MeetingEntity> {

    private MeetingBeanFactory meetingBeanFactory;

    @Autowired
    public void setMeetingBeanFactory(MeetingBeanFactory meetingBeanFactory) {
        this.meetingBeanFactory = meetingBeanFactory;
    }

    public List<MeetingVo> meetingEntity2DetailedVo(List<? extends MeetingEntity> meetingEntityList) {
        List<MeetingVo> meetingDetailsList = new LinkedList<>();
        if(meetingEntityList != null && !meetingEntityList.isEmpty()) {
            for(MeetingEntity entity : meetingEntityList) {
                MeetingVo vo = this.meetingEntity2DetailedVo(entity);
                meetingDetailsList.add(vo);
            }
        }
        return meetingDetailsList;
    }

    public MeetingVo meetingEntity2DetailedVo(MeetingEntity meetingEntity) {
        Optional<? extends MeetingEntity2VoConverter> optionalMeetingEntity2VoConverter = this.meetingBeanFactory.getMeetingEntity2VoConverter("");
        MeetingEntity2VoConverter meetingEntity2VoConverter = optionalMeetingEntity2VoConverter.get();
        if(meetingEntity != null) {
            MeetingVo vo = meetingEntity2VoConverter.convert(meetingEntity);
            log.debug("Converting {} to {}", meetingEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "meeting entity is null" });
    }

    public PickUpVo pickUpEntity2DetailedVo(PickUpEntity pickUpEntity) {
        Optional<? extends MeetingEntity2VoConverter> optionalMeetingEntity2VoConverter = this.meetingBeanFactory.getMeetingEntity2VoConverter(MeetingType.PICK_UP.name());
        PickUpEntity2VoConverter pickUpEntity2VoConverter = (PickUpEntity2VoConverter) optionalMeetingEntity2VoConverter.get();
        if(pickUpEntity != null) {
            PickUpVo vo = pickUpEntity2VoConverter.convert(pickUpEntity);
            log.debug("Converting {} to {}", pickUpEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "pickUp entity is null" });
    }

    public List<PickUpVo> pickUpEntity2DetailedVo(List<PickUpEntity> pickUpEntityList) {
        List<PickUpVo> pickUpDetailsList = new LinkedList<>();
        if(pickUpEntityList != null && !pickUpEntityList.isEmpty()) {
            for(PickUpEntity entity : pickUpEntityList) {
                PickUpVo vo = this.pickUpEntity2DetailedVo(entity);
                pickUpDetailsList.add(vo);
            }
        }
        return pickUpDetailsList;
    }

    public DeliveryVo deliveryEntity2DetailedVo(DeliveryEntity deliveryEntity) {
        Optional<? extends MeetingEntity2VoConverter> optionalMeetingEntity2VoConverter = this.meetingBeanFactory.getMeetingEntity2VoConverter(MeetingType.DELIVERY.name());
        DeliveryEntity2VoConverter deliveryEntity2VoConverter = (DeliveryEntity2VoConverter) optionalMeetingEntity2VoConverter.get();
        if(deliveryEntity != null) {
            DeliveryVo vo = deliveryEntity2VoConverter.convert(deliveryEntity);
            log.debug("Converting {} to {}", deliveryEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "delivery entity is null" });
    }

    public List<DeliveryVo> deliveryEntity2DetailedVo(List<DeliveryEntity> deliveryEntityList) {
        List<DeliveryVo> deliveryDetailsList = new LinkedList<>();
        if(deliveryEntityList != null && !deliveryEntityList.isEmpty()) {
            for(DeliveryEntity entity : deliveryEntityList) {
                DeliveryVo vo = this.deliveryEntity2DetailedVo(entity);
                deliveryDetailsList.add(vo);
            }
        }
        return deliveryDetailsList;
    }
}
