package com.teenthofabud.restaurant.solution.engagement.checkin.service.impl;

import com.teenthofabud.restaurant.solution.engagement.checkin.converter.WalkInDto2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.WalkInEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.WalkInForm2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.WalkInEntitySelfMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.WalkInForm2EntityMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.WalkInRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.WalkInService;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.WalkInDtoValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.WalkInFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.WalkInFormValidator;
import com.teenthofabud.restaurant.solution.engagement.constants.CheckInType;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.util.StringUtils;

import java.util.*;

@Slf4j
public class WalkInServiceImpl extends CheckInServiceImpl<WalkInFormValidator, WalkInFormRelaxedValidator, WalkInDtoValidator, WalkInRepository,
        WalkInEntitySelfMapper, WalkInForm2EntityMapper, WalkInForm2EntityConverter, WalkInEntity2VoConverter, WalkInDto2EntityConverter>
        implements WalkInService<WalkInForm, WalkInVo> {

    @Override
    public WalkInFormValidator getCheckInFormValidator() {
        return (WalkInFormValidator) this.checkInBeanFactory.getCheckInFormValidator(CheckInType.WALK_IN.name()).get();
    }

    @Override
    public WalkInFormRelaxedValidator getCheckInFormRelaxedValidator() {
        return (WalkInFormRelaxedValidator) this.checkInBeanFactory.getCheckInFormRelaxedValidator(CheckInType.WALK_IN.name()).get();
    }

    @Override
    public WalkInDtoValidator getCheckInDtoValidator() {
        return (WalkInDtoValidator) this.checkInBeanFactory.getCheckInDtoValidator(CheckInType.WALK_IN.name()).get();
    }

    @Override
    public WalkInRepository getCheckInRepository() {
        return (WalkInRepository) this.checkInBeanFactory.getCheckInRepository(CheckInType.WALK_IN.name()).get();
    }

    @Override
    public WalkInEntitySelfMapper getCheckInEntitySelfMapper() {
        return (WalkInEntitySelfMapper) this.checkInBeanFactory.getCheckInEntitySelfMapper(CheckInType.WALK_IN.name()).get();
    }

    @Override
    public WalkInForm2EntityMapper getCheckInForm2EntityMapper() {
        return (WalkInForm2EntityMapper) this.checkInBeanFactory.getCheckInForm2EntityMapper(CheckInType.WALK_IN.name()).get();
    }

    @Override
    public WalkInForm2EntityConverter getCheckInForm2EntityConverter() {
        return (WalkInForm2EntityConverter) this.checkInBeanFactory.getCheckInForm2EntityConverter(CheckInType.WALK_IN.name()).get();
    }

    @Override
    public WalkInEntity2VoConverter getCheckInEntity2VoConverter() {
        return (WalkInEntity2VoConverter) this.checkInBeanFactory.getCheckInEntity2VoConverter(CheckInType.WALK_IN.name()).get();
    }

    @Override
    public WalkInDto2EntityConverter getCheckInDto2EntityConverter() {
        return (WalkInDto2EntityConverter) this.checkInBeanFactory.getCheckInDto2EntityConverter(CheckInType.WALK_IN.name()).get();
    }

    @Override
    public List<WalkInVo> retrieveAllMatchingWalkInDetailsByCriteria(Optional<String> optionalName, Optional<String> optionalPhoneNumber, Optional<String> optionalEmailId) throws CheckInException {
        if(optionalName.isEmpty() && optionalPhoneNumber.isEmpty() && optionalEmailId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String phoneNumber = optionalPhoneNumber.isPresent() ? optionalPhoneNumber.get() : "";
        String emailId = optionalEmailId.isPresent() ? optionalEmailId.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(phoneNumber)) && StringUtils.isEmpty(StringUtils.trimWhitespace(emailId))) {
            log.debug("All search parameters are empty");
        }
        List<WalkInVo> matchedWalkInList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        WalkInEntity entity = new WalkInEntity(new CheckInEntity());
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
            entity.setPhoneNumber(phoneNumber);
            matcherCriteria = matcherCriteria.withMatcher("phoneNumber", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(emailId))) {
            log.debug("emailId {} is valid", name);
            providedFilters.put("emailId", name);
            entity.setEmailId(emailId);
            matcherCriteria = matcherCriteria.withMatcher("emailId", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<WalkInEntity> walkInEntityExample = Example.of(entity, matcherCriteria);
        List<WalkInEntity> walkInEntityList = this.getCheckInRepository().findAll(walkInEntityExample);
        matchedWalkInList = super.engagementServiceHelper.walkInEntity2DetailedVo(walkInEntityList);
        log.info("Found {} WalkInVo matching with provided parameters : {}", matchedWalkInList.size(), providedFilters);
        log.info("No WalkInVo available matching with provided parameters : {}", matchedWalkInList.size(), providedFilters);
        return matchedWalkInList;
    }
}