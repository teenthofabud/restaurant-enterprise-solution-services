package com.teenthofabud.restaurant.solution.engagement.checkin.factory;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.CheckInRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.ReservationRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.WalkInRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.*;
import com.teenthofabud.restaurant.solution.engagement.constants.CheckInType;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
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
                break;
        }
        return checkInFormRelaxedValidator;
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }
}
