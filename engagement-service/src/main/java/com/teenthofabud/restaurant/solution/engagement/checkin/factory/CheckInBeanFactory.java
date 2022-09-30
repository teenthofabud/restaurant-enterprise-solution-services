package com.teenthofabud.restaurant.solution.engagement.checkin.factory;

import com.teenthofabud.restaurant.solution.engagement.checkin.converter.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.CheckInRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.ReservationRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.WalkInRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.*;
import com.teenthofabud.restaurant.solution.engagement.constants.CheckInType;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class CheckInBeanFactory implements ApplicationContextAware {

    public Optional<CheckInRepository<? extends CheckInEntity>> getCheckInRepository(String checkInType) {
        Optional<CheckInRepository<? extends CheckInEntity>> checkInRepository = Optional.empty();
        CheckInType type = CheckInType.valueOf(checkInType);
        switch (type) {
            case RESERVATION:
                checkInRepository = Optional.of(applicationContext.getBean(ReservationRepository.class));
                break;
            case WALK_IN:
                checkInRepository = Optional.of(applicationContext.getBean(WalkInRepository.class));
                break;
            default:
                log.error("CheckIn type: {} not supported");
                break;
        }
        return checkInRepository;
    }

    private ApplicationContext applicationContext;

    public Optional<? extends CheckInDtoValidator> getCheckInDtoValidator(String checkInType) {
        Optional<? extends CheckInDtoValidator> checkInDtoValidator = Optional.empty();
        CheckInType type = CheckInType.valueOf(checkInType);
        switch (type) {
            case RESERVATION:
                checkInDtoValidator = Optional.of(applicationContext.getBean(ReservationDtoValidator.class));
                break;
            case WALK_IN:
                checkInDtoValidator = Optional.of(applicationContext.getBean(WalkInDtoValidator.class));
                break;
            default:
                log.error("CheckIn type: {} not supported");
                break;
        }
        return checkInDtoValidator;
    }

    public Optional<? extends CheckInFormValidator> getCheckInFormValidator(String checkInType) {
        Optional<? extends CheckInFormValidator> checkInFormValidator = Optional.empty();
        CheckInType type = CheckInType.valueOf(checkInType);
        switch (type) {
            case RESERVATION:
                checkInFormValidator = Optional.of(applicationContext.getBean(ReservationFormValidator.class));
                break;
            case WALK_IN:
                checkInFormValidator = Optional.of(applicationContext.getBean(WalkInFormValidator.class));
                break;
            default:
                log.error("CheckIn type: {} not supported");
                break;
        }
        return checkInFormValidator;
    }

    public Optional<? extends CheckInFormRelaxedValidator> getCheckInFormRelaxedValidator(String checkInType) {
        Optional<? extends CheckInFormRelaxedValidator> checkInFormRelaxedValidator = Optional.empty();
        CheckInType type = CheckInType.valueOf(checkInType);
        switch (type) {
            case RESERVATION:
                checkInFormRelaxedValidator = Optional.of(applicationContext.getBean(ReservationFormRelaxedValidator.class));
                break;
            case WALK_IN:
                checkInFormRelaxedValidator = Optional.of(applicationContext.getBean(WalkInFormRelaxedValidator.class));
                break;
            default:
                log.error("CheckIn type: {} not supported");
                break;
        }
        return checkInFormRelaxedValidator;
    }

    public Optional<? extends CheckInEntitySelfMapper> getCheckInEntitySelfMapper(String checkInType) {
        Optional<? extends CheckInEntitySelfMapper> checkInEntitySelfMapper = Optional.empty();
        CheckInType type = CheckInType.valueOf(checkInType);
        switch (type) {
            case RESERVATION:
                checkInEntitySelfMapper = Optional.of(applicationContext.getBean(ReservationEntitySelfMapper.class));
                break;
            case WALK_IN:
                checkInEntitySelfMapper = Optional.of(applicationContext.getBean(WalkInEntitySelfMapper.class));
                break;
            default:
                log.error("CheckIn type: {} not supported");
                break;
        }
        return checkInEntitySelfMapper;
    }

    public Optional<? extends CheckInForm2EntityMapper> getCheckInForm2EntityMapper(String checkInType) {
        Optional<? extends CheckInForm2EntityMapper> checkInForm2EntityMapper = Optional.empty();
        CheckInType type = CheckInType.valueOf(checkInType);
        switch (type) {
            case RESERVATION:
                checkInForm2EntityMapper = Optional.of(applicationContext.getBean(ReservationForm2EntityMapper.class));
                break;
            case WALK_IN:
                checkInForm2EntityMapper = Optional.of(applicationContext.getBean(WalkInForm2EntityMapper.class));
                break;
            default:
                log.error("CheckIn type: {} not supported");
                break;
        }
        return checkInForm2EntityMapper;
    }

    public Optional<? extends CheckInForm2EntityConverter> getCheckInForm2EntityConverter(String checkInType) {
        Optional<? extends CheckInForm2EntityConverter> checkInForm2EntityConverter = Optional.empty();
        CheckInType type = CheckInType.valueOf(checkInType);
        switch (type) {
            case RESERVATION:
                checkInForm2EntityConverter = Optional.of(applicationContext.getBean(ReservationForm2EntityConverter.class));
                break;
            case WALK_IN:
                checkInForm2EntityConverter = Optional.of(applicationContext.getBean(WalkInForm2EntityConverter.class));
                break;
            default:
                log.error("CheckIn type: {} not supported");
                break;
        }
        return checkInForm2EntityConverter;
    }

    public Optional<? extends CheckInDto2EntityConverter> getCheckInDto2EntityConverter(String checkInType) {
        Optional<? extends CheckInDto2EntityConverter> checkInDto2EntityConverter = Optional.empty();
        CheckInType type = CheckInType.valueOf(checkInType);
        switch (type) {
            case RESERVATION:
                checkInDto2EntityConverter = Optional.of(applicationContext.getBean(ReservationDto2EntityConverter.class));
                break;
            case WALK_IN:
                checkInDto2EntityConverter = Optional.of(applicationContext.getBean(WalkInDto2EntityConverter.class));
                break;
            default:
                log.error("CheckIn type: {} not supported");
                break;
        }
        return checkInDto2EntityConverter;
    }

    public Optional<? extends CheckInEntity2VoConverter> getCheckInEntity2VoConverter(String checkInType) {
        Optional<? extends CheckInEntity2VoConverter> checkInEntity2VoConverter = Optional.empty();
        CheckInType type = CheckInType.valueOf(checkInType);
        switch (type) {
            case RESERVATION:
                checkInEntity2VoConverter = Optional.of(applicationContext.getBean(ReservationEntity2VoConverter.class));
                break;
            case WALK_IN:
                checkInEntity2VoConverter = Optional.of(applicationContext.getBean(WalkInEntity2VoConverter.class));
                break;
            default:
                log.error("CheckIn type: {} not supported");
                break;
        }
        return checkInEntity2VoConverter;
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }
}
