package com.teenthofabud.restaurant.solution.encounter.meeting.factory;

import com.teenthofabud.restaurant.solution.encounter.delivery.converter.DeliveryDto2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.delivery.converter.DeliveryEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.delivery.converter.DeliveryForm2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.delivery.mapper.DeliveryEntitySelfMapper;
import com.teenthofabud.restaurant.solution.encounter.delivery.mapper.DeliveryForm2EntityMapper;
import com.teenthofabud.restaurant.solution.encounter.delivery.repository.DeliveryRepository;
import com.teenthofabud.restaurant.solution.encounter.delivery.validator.DeliveryDtoValidator;
import com.teenthofabud.restaurant.solution.encounter.delivery.validator.DeliveryFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.encounter.delivery.validator.DeliveryFormValidator;
import com.teenthofabud.restaurant.solution.encounter.meeting.constants.MeetingType;
import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingDto2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingForm2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import com.teenthofabud.restaurant.solution.encounter.meeting.mapper.MeetingEntitySelfMapper;
import com.teenthofabud.restaurant.solution.encounter.meeting.mapper.MeetingForm2EntityMapper;
import com.teenthofabud.restaurant.solution.encounter.meeting.repository.MeetingRepository;
import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingDtoValidator;
import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingFormValidator;
import com.teenthofabud.restaurant.solution.encounter.pickup.converter.PickUpDto2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.converter.PickUpEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.converter.PickUpForm2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.mapper.PickUpEntitySelfMapper;
import com.teenthofabud.restaurant.solution.encounter.pickup.mapper.PickUpForm2EntityMapper;
import com.teenthofabud.restaurant.solution.encounter.pickup.repository.PickUpRepository;
import com.teenthofabud.restaurant.solution.encounter.pickup.validator.PickUpDtoValidator;
import com.teenthofabud.restaurant.solution.encounter.pickup.validator.PickUpFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.encounter.pickup.validator.PickUpFormValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class MeetingBeanFactory implements ApplicationContextAware {

    public Optional<MeetingRepository<? extends MeetingEntity>> getMeetingRepository(String meetingType) {
        Optional<MeetingRepository<? extends MeetingEntity>> meetingRepository = Optional.empty();
        MeetingType type = MeetingType.valueOf(meetingType);
        switch (type) {
            case DELIVERY:
                meetingRepository = Optional.of(applicationContext.getBean(DeliveryRepository.class));
                break;
            case PICK_UP:
                meetingRepository = Optional.of(applicationContext.getBean(PickUpRepository.class));
                break;
            default:
                log.error("Meeting type: {} not supported");
                meetingRepository = Optional.of(applicationContext.getBean(MeetingRepository.class));
                break;
        }
        return meetingRepository;
    }

    private ApplicationContext applicationContext;

    public Optional<? extends MeetingDtoValidator> getMeetingDtoValidator(String meetingType) {
        Optional<? extends MeetingDtoValidator> meetingDtoValidator = Optional.empty();
        MeetingType type = MeetingType.valueOf(meetingType);
        switch (type) {
            case DELIVERY:
                meetingDtoValidator = Optional.of(applicationContext.getBean(DeliveryDtoValidator.class));
                break;
            case PICK_UP:
                meetingDtoValidator = Optional.of(applicationContext.getBean(PickUpDtoValidator.class));
                break;
            default:
                log.error("Meeting type: {} not supported");
                break;
        }
        return meetingDtoValidator;
    }

    public Optional<? extends MeetingFormValidator> getMeetingFormValidator(String meetingType) {
        Optional<? extends MeetingFormValidator> meetingFormValidator = Optional.empty();
        MeetingType type = MeetingType.valueOf(meetingType);
        switch (type) {
            case DELIVERY:
                meetingFormValidator = Optional.of(applicationContext.getBean(DeliveryFormValidator.class));
                break;
            case PICK_UP:
                meetingFormValidator = Optional.of(applicationContext.getBean(PickUpFormValidator.class));
                break;
            default:
                log.error("Meeting type: {} not supported");
                break;
        }
        return meetingFormValidator;
    }

    public Optional<? extends MeetingFormRelaxedValidator> getMeetingFormRelaxedValidator(String meetingType) {
        Optional<? extends MeetingFormRelaxedValidator> meetingFormRelaxedValidator = Optional.empty();
        MeetingType type = MeetingType.valueOf(meetingType);
        switch (type) {
            case DELIVERY:
                meetingFormRelaxedValidator = Optional.of(applicationContext.getBean(DeliveryFormRelaxedValidator.class));
                break;
            case PICK_UP:
                meetingFormRelaxedValidator = Optional.of(applicationContext.getBean(PickUpFormRelaxedValidator.class));
                break;
            default:
                log.error("Meeting type: {} not supported");
                break;
        }
        return meetingFormRelaxedValidator;
    }

    public Optional<? extends MeetingEntitySelfMapper> getMeetingEntitySelfMapper(String meetingType) {
        Optional<? extends MeetingEntitySelfMapper> meetingEntitySelfMapper = Optional.empty();
        MeetingType type = MeetingType.valueOf(meetingType);
        switch (type) {
            case DELIVERY:
                meetingEntitySelfMapper = Optional.of(applicationContext.getBean(DeliveryEntitySelfMapper.class));
                break;
            case PICK_UP:
                meetingEntitySelfMapper = Optional.of(applicationContext.getBean(PickUpEntitySelfMapper.class));
                break;
            default:
                log.error("Meeting type: {} not supported");
                break;
        }
        return meetingEntitySelfMapper;
    }

    public Optional<? extends MeetingForm2EntityMapper> getMeetingForm2EntityMapper(String meetingType) {
        Optional<? extends MeetingForm2EntityMapper> meetingForm2EntityMapper = Optional.empty();
        MeetingType type = MeetingType.valueOf(meetingType);
        switch (type) {
            case DELIVERY:
                meetingForm2EntityMapper = Optional.of(applicationContext.getBean(DeliveryForm2EntityMapper.class));
                break;
            case PICK_UP:
                meetingForm2EntityMapper = Optional.of(applicationContext.getBean(PickUpForm2EntityMapper.class));
                break;
            default:
                log.error("Meeting type: {} not supported");
                break;
        }
        return meetingForm2EntityMapper;
    }

    public Optional<? extends MeetingForm2EntityConverter> getMeetingForm2EntityConverter(String meetingType) {
        Optional<? extends MeetingForm2EntityConverter> meetingForm2EntityConverter = Optional.empty();
        MeetingType type = MeetingType.valueOf(meetingType);
        switch (type) {
            case DELIVERY:
                meetingForm2EntityConverter = Optional.of(applicationContext.getBean(DeliveryForm2EntityConverter.class));
                break;
            case PICK_UP:
                meetingForm2EntityConverter = Optional.of(applicationContext.getBean(PickUpForm2EntityConverter.class));
                break;
            default:
                log.error("Meeting type: {} not supported");
                break;
        }
        return meetingForm2EntityConverter;
    }

    public Optional<? extends MeetingDto2EntityConverter> getMeetingDto2EntityConverter(String meetingType) {
        Optional<? extends MeetingDto2EntityConverter> meetingDto2EntityConverter = Optional.empty();
        MeetingType type = MeetingType.valueOf(meetingType);
        switch (type) {
            case DELIVERY:
                meetingDto2EntityConverter = Optional.of(applicationContext.getBean(DeliveryDto2EntityConverter.class));
                break;
            case PICK_UP:
                meetingDto2EntityConverter = Optional.of(applicationContext.getBean(PickUpDto2EntityConverter.class));
                break;
            default:
                log.error("Meeting type: {} not supported");
                break;
        }
        return meetingDto2EntityConverter;
    }

    public Optional<? extends MeetingEntity2VoConverter> getMeetingEntity2VoConverter(String meetingType) {
        Optional<? extends MeetingEntity2VoConverter> meetingEntity2VoConverter = Optional.empty();
        MeetingType type = MeetingType.valueOf(meetingType);
        switch (type) {
            case DELIVERY:
                meetingEntity2VoConverter = Optional.of(applicationContext.getBean(DeliveryEntity2VoConverter.class));
                break;
            case PICK_UP:
                meetingEntity2VoConverter = Optional.of(applicationContext.getBean(PickUpEntity2VoConverter.class));
                break;
            default:
                log.error("Meeting type: {} not supported");
                break;
        }
        return meetingEntity2VoConverter;
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }
}
