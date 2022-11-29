package com.teenthofabud.restaurant.solution.encounter.pickup.service.impl;

import com.teenthofabud.restaurant.solution.encounter.meeting.constants.MeetingType;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import com.teenthofabud.restaurant.solution.encounter.meeting.service.impl.MeetingServiceImpl;
import com.teenthofabud.restaurant.solution.encounter.pickup.converter.PickUpDto2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.converter.PickUpEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.converter.PickUpForm2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpEntity;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpForm;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpVo;
import com.teenthofabud.restaurant.solution.encounter.pickup.mapper.PickUpEntitySelfMapper;
import com.teenthofabud.restaurant.solution.encounter.pickup.mapper.PickUpForm2EntityMapper;
import com.teenthofabud.restaurant.solution.encounter.pickup.repository.PickUpRepository;
import com.teenthofabud.restaurant.solution.encounter.pickup.service.PickUpService;
import com.teenthofabud.restaurant.solution.encounter.pickup.validator.PickUpDtoValidator;
import com.teenthofabud.restaurant.solution.encounter.pickup.validator.PickUpFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.encounter.pickup.validator.PickUpFormValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.*;

@Slf4j
@Service
public class PickUpServiceImpl extends MeetingServiceImpl<PickUpFormValidator, PickUpFormRelaxedValidator, PickUpDtoValidator, PickUpRepository,
        PickUpEntitySelfMapper, PickUpForm2EntityMapper, PickUpForm2EntityConverter, PickUpEntity2VoConverter, PickUpDto2EntityConverter>
        implements PickUpService<PickUpForm, PickUpVo> {

    @Override
    public PickUpFormValidator getMeetingFormValidator() {
        return (PickUpFormValidator) this.meetingBeanFactory.getMeetingFormValidator(MeetingType.PICK_UP.name()).get();
    }

    @Override
    public PickUpFormRelaxedValidator getMeetingFormRelaxedValidator() {
        return (PickUpFormRelaxedValidator) this.meetingBeanFactory.getMeetingFormRelaxedValidator(MeetingType.PICK_UP.name()).get();
    }

    @Override
    public PickUpDtoValidator getMeetingDtoValidator() {
        return (PickUpDtoValidator) this.meetingBeanFactory.getMeetingDtoValidator(MeetingType.PICK_UP.name()).get();
    }

    @Override
    public PickUpRepository getMeetingRepository() {
        return (PickUpRepository) this.meetingBeanFactory.getMeetingRepository(MeetingType.PICK_UP.name()).get();
    }

    @Override
    public PickUpEntitySelfMapper getMeetingEntitySelfMapper() {
        return (PickUpEntitySelfMapper) this.meetingBeanFactory.getMeetingEntitySelfMapper(MeetingType.PICK_UP.name()).get();
    }

    @Override
    public PickUpForm2EntityMapper getMeetingForm2EntityMapper() {
        return (PickUpForm2EntityMapper) this.meetingBeanFactory.getMeetingForm2EntityMapper(MeetingType.PICK_UP.name()).get();
    }

    @Override
    public PickUpForm2EntityConverter getMeetingForm2EntityConverter() {
        return (PickUpForm2EntityConverter) this.meetingBeanFactory.getMeetingForm2EntityConverter(MeetingType.PICK_UP.name()).get();
    }

    @Override
    public PickUpEntity2VoConverter getMeetingEntity2VoConverter() {
        return (PickUpEntity2VoConverter) this.meetingBeanFactory.getMeetingEntity2VoConverter(MeetingType.PICK_UP.name()).get();
    }

    @Override
    public PickUpDto2EntityConverter getMeetingDto2EntityConverter() {
        return (PickUpDto2EntityConverter) this.meetingBeanFactory.getMeetingDto2EntityConverter(MeetingType.PICK_UP.name()).get();
    }

    @Override
    public List<PickUpVo> retrieveAllMatchingPickUpDetailsByCriteria(Optional<String> optionalName, Optional<String> optionalPhoneNumber)
            throws MeetingException {
        if(optionalName.isEmpty() && optionalPhoneNumber.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String phoneNumber = optionalPhoneNumber.isPresent() ? optionalPhoneNumber.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(phoneNumber))) {
            log.debug("All search parameters are empty");
        }
        List<PickUpVo> matchedPickUpList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        PickUpEntity entity = new PickUpEntity(new MeetingEntity());
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            log.debug("name {} is valid", name);
            providedFilters.put("name", name);
            entity.setName(name);
            matcherCriteria = matcherCriteria.withMatcher("name", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(phoneNumber))) {
            log.debug("phoneNumber {} is valid", name);
            providedFilters.put("phoneNumber", name);
            entity.setPhoneNo(phoneNumber);
            matcherCriteria = matcherCriteria.withMatcher("phoneNumber", match -> match.exact());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<PickUpEntity> pickUpEntityExample = Example.of(entity, matcherCriteria);
        List<PickUpEntity> pickUpEntityList = this.getMeetingRepository().findAll(pickUpEntityExample);
        matchedPickUpList = super.encounterServiceHelper.pickUpEntity2DetailedVo(pickUpEntityList);
        log.info("Found {} PickUpVo matching with provided parameters : {}", matchedPickUpList.size(), providedFilters);
        log.info("No PickUpVo available matching with provided parameters : {}", matchedPickUpList.size(), providedFilters);
        return matchedPickUpList;
    }
}