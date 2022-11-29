package com.teenthofabud.restaurant.solution.encounter.delivery.service.impl;

import com.teenthofabud.restaurant.solution.encounter.delivery.converter.DeliveryDto2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.delivery.converter.DeliveryEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.delivery.converter.DeliveryForm2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryEntity;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryForm;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryVo;
import com.teenthofabud.restaurant.solution.encounter.delivery.mapper.DeliveryEntitySelfMapper;
import com.teenthofabud.restaurant.solution.encounter.delivery.mapper.DeliveryForm2EntityMapper;
import com.teenthofabud.restaurant.solution.encounter.delivery.repository.DeliveryRepository;
import com.teenthofabud.restaurant.solution.encounter.delivery.service.DeliveryService;
import com.teenthofabud.restaurant.solution.encounter.delivery.validator.DeliveryDtoValidator;
import com.teenthofabud.restaurant.solution.encounter.delivery.validator.DeliveryFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.encounter.delivery.validator.DeliveryFormValidator;
import com.teenthofabud.restaurant.solution.encounter.meeting.constants.MeetingType;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import com.teenthofabud.restaurant.solution.encounter.meeting.service.impl.MeetingServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.util.StringUtils;

import java.util.*;

@Slf4j
public class DeliveryServiceImpl extends MeetingServiceImpl<DeliveryFormValidator, DeliveryFormRelaxedValidator, DeliveryDtoValidator, DeliveryRepository,
        DeliveryEntitySelfMapper, DeliveryForm2EntityMapper, DeliveryForm2EntityConverter, DeliveryEntity2VoConverter, DeliveryDto2EntityConverter>
        implements DeliveryService<DeliveryForm, DeliveryVo> {

    @Override
    public DeliveryFormValidator getMeetingFormValidator() {
        return (DeliveryFormValidator) this.meetingBeanFactory.getMeetingFormValidator(MeetingType.DELIVERY.name()).get();
    }

    @Override
    public DeliveryFormRelaxedValidator getMeetingFormRelaxedValidator() {
        return (DeliveryFormRelaxedValidator) this.meetingBeanFactory.getMeetingFormRelaxedValidator(MeetingType.DELIVERY.name()).get();
    }

    @Override
    public DeliveryDtoValidator getMeetingDtoValidator() {
        return (DeliveryDtoValidator) this.meetingBeanFactory.getMeetingDtoValidator(MeetingType.DELIVERY.name()).get();
    }

    @Override
    public DeliveryRepository getMeetingRepository() {
        return (DeliveryRepository) this.meetingBeanFactory.getMeetingRepository(MeetingType.DELIVERY.name()).get();
    }

    @Override
    public DeliveryEntitySelfMapper getMeetingEntitySelfMapper() {
        return (DeliveryEntitySelfMapper) this.meetingBeanFactory.getMeetingEntitySelfMapper(MeetingType.DELIVERY.name()).get();
    }

    @Override
    public DeliveryForm2EntityMapper getMeetingForm2EntityMapper() {
        return (DeliveryForm2EntityMapper) this.meetingBeanFactory.getMeetingForm2EntityMapper(MeetingType.DELIVERY.name()).get();
    }

    @Override
    public DeliveryForm2EntityConverter getMeetingForm2EntityConverter() {
        return (DeliveryForm2EntityConverter) this.meetingBeanFactory.getMeetingForm2EntityConverter(MeetingType.DELIVERY.name()).get();
    }

    @Override
    public DeliveryEntity2VoConverter getMeetingEntity2VoConverter() {
        return (DeliveryEntity2VoConverter) this.meetingBeanFactory.getMeetingEntity2VoConverter(MeetingType.DELIVERY.name()).get();
    }

    @Override
    public DeliveryDto2EntityConverter getMeetingDto2EntityConverter() {
        return (DeliveryDto2EntityConverter) this.meetingBeanFactory.getMeetingDto2EntityConverter(MeetingType.DELIVERY.name()).get();
    }

    @Override
    public List<DeliveryVo> retrieveAllMatchingDeliveryDetailsByCriteria(Optional<String> optionalOrderId) 
            throws MeetingException {
        if(optionalOrderId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String orderId = optionalOrderId.isPresent() ? optionalOrderId.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(orderId))) {
            log.debug("All search parameters are empty");
        }
        List<DeliveryVo> matchedDeliveryList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        DeliveryEntity entity = new DeliveryEntity(new MeetingEntity());
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(orderId))) {
            log.debug("orderId {} is valid", orderId);
            providedFilters.put("orderId", orderId);
            entity.setOrderId(orderId);
            matcherCriteria = matcherCriteria.withMatcher("orderId", match -> match.exact());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<DeliveryEntity> deliveryEntityExample = Example.of(entity, matcherCriteria);
        List<DeliveryEntity> deliveryEntityList = this.getMeetingRepository().findAll(deliveryEntityExample);
        matchedDeliveryList = super.encounterServiceHelper.deliveryEntity2DetailedVo(deliveryEntityList);
        log.info("Found {} DeliveryVo matching with provided parameters : {}", matchedDeliveryList.size(), providedFilters);
        log.info("No DeliveryVo available matching with provided parameters : {}", matchedDeliveryList.size(), providedFilters);
        return matchedDeliveryList;
    }
}