package com.teenthofabud.restaurant.solution.establishmentarea.floor.service.impl;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.converter.FloorDto2EntityConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.converter.FloorEntity2VoConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.converter.FloorForm2EntityConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.*;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.mapper.FloorEntitySelfMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.mapper.FloorForm2EntityMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.repository.FloorRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.service.FloorService;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.validator.FloorFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.validator.FloorFormValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.stream.Collectors;

import static org.springframework.data.domain.ExampleMatcher.GenericPropertyMatchers.endsWith;

@Component
@Slf4j
public class FloorServiceImpl implements FloorService {

    private static final Comparator<FloorVo> CMP_BY_FLOOR_NAME = (s1, s2) -> {
        return s1.getFlrName().compareTo(s2.getFlrName());
    };

    private FloorRepository floorRepository;
    
    private FloorEntity2VoConverter entity2VoConverter;
    private FloorForm2EntityConverter form2EntityConverter;
    private FloorDto2EntityConverter dto2EntityConverter;
    
    private FloorFormValidator formValidator;
    private FloorFormRelaxedValidator floorFormRelaxedValidator;
    
    private FloorForm2EntityMapper form2EntityMapper;
    private FloorEntitySelfMapper entitySelfMapper;

    @Autowired
    public void setForm2EntityMapper(FloorForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(FloorEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setFloorFormRelaxedValidator(FloorFormRelaxedValidator floorFormRelaxedValidator) {
        this.floorFormRelaxedValidator = floorFormRelaxedValidator;
    }

    @Autowired
    public void setEntity2VoConverter(FloorEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setForm2EntityConverter(FloorForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setDto2EntityConverter(FloorDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setFormValidator(FloorFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    @Autowired
    public void setFloorRepository(FloorRepository floorRepository) {
        this.floorRepository = floorRepository;
    }

    @Transactional
    @Override
    public String createFloor(FloorForm form) throws FloorException{
        log.info("Creating new FloorEntity");
        log.debug("Form details: {}", form);
        log.debug("Validating provided attributes of FloorForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("FloorForm has {} errors", err.getErrorCount());
            EstablishmentAreaErrorCode ec = EstablishmentAreaErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("FloorForm error detail: {}", ec);
            throw new FloorException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of FloorForm are valid");

        FloorEntity expectedEntity = form2EntityConverter.convert(form);

        log.debug("Saving {}", expectedEntity);
        FloorEntity actualEntity = floorRepository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist FloorForm details" });
        }
        log.info("Created new FloorForm with id: {}", actualEntity.getFlrId());
        return actualEntity.getFlrId().toString();
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<FloorVo> retrieveListOfAllFloors() {
        log.info("Retrieving all the FloorEntities");
        List<FloorEntity> floorEntities = floorRepository.findAll();
        List<FloorVo> naturallyOrderedList = new ArrayList<>();
        for(FloorEntity entity : floorEntities) {
            FloorVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedList.add(dto);
        }
        return naturallyOrderedList.stream()
                .sorted(CMP_BY_FLOOR_NAME)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public FloorVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel)
            throws FloorException {
        log.info("Requesting FloorEntity by id: {}", id);
        Long floorId = parseFloorId(id);
        Optional<FloorEntity> optEntity = floorRepository.findById(floorId);
        if(optEntity.isEmpty()) {
            log.debug("No FloorEntity found by id: {}", id);
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND,
                    new Object[] { "id", String.valueOf(id) });
        }
        FloorEntity entity = optEntity.get();
        FloorVo vo = entity2VoConverter.convert(entity);
        log.info("Found FloorVo by id: {}", id);
        return vo;
    }

    private Long parseFloorId(String id) throws FloorException {
        Long floorId = null;
        try {
            floorId = Long.parseLong(id);
            log.debug("Parsed id {} to floor id {} in numeric format", id, floorId);
            if(floorId <= 0) {
                throw new NumberFormatException("floor id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse floor id", e);
            log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_ID_INVALID.getValue(), id);
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID,
                    new Object[] { "id", id });
        }
        return floorId;
    }

    private List<FloorVo> entity2DetailedVoList(List<FloorEntity> floorEntityList) {
        List<FloorVo> floorDetailsList = new ArrayList<>(floorEntityList.size());
        for(FloorEntity entity : floorEntityList) {
            FloorVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            floorDetailsList.add(vo);
        }
        return floorDetailsList;
    }

    @Transactional
    @Override
    public List<FloorVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalFloorName) throws FloorException {

        String floorName = optionalFloorName.get();
        log.info("Requesting FloorEntity by floor name: {}", floorName);

        List<FloorVo> matchedAccountList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        FloorEntity entity = new FloorEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(floorName))) {
            log.debug("floorName {} is valid", floorName);
            providedFilters.put("flrName", floorName);
            entity.setFlrName(floorName);
            matcherCriteria = matcherCriteria.withMatcher("flrName", ExampleMatcher.GenericPropertyMatchers.contains());
        }
        Example<FloorEntity> accountEntityExample = Example.of(entity, matcherCriteria);
        List<FloorEntity> accountEntityList = floorRepository.findAll(accountEntityExample);
        if(accountEntityList != null && !accountEntityList.isEmpty()) {
            matchedAccountList = entity2DetailedVoList(accountEntityList);
            log.info("Found {} FloorVo matching with provided parameters : {}", matchedAccountList.size(), providedFilters);
        } else
            log.info("Found no FloorVo available matching with provided parameters : {}", providedFilters);
        return matchedAccountList;
    }

    @Transactional
    @Override
    public void updateFloor(String id, FloorForm form) throws FloorException {
        log.info("Updating FloorEntity by id: {}", id);
        Long floorId = parseFloorId(id);
        Optional<FloorEntity> optEntity = floorRepository.findById(floorId);
        if(optEntity.isEmpty()) {
            log.debug("No FloorEntity found by id: {}", id);
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND,
                    new Object[] { "id", String.valueOf(id) });
        }
        log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_ID_VALID.getValue(), id);

        FloorEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("FloorEntity is inactive with id: {}", id);
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("FloorEntity is active with id: {}", id);

        if(form == null) {
            log.debug("FloorForm is null");
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED, 
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of FloorForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = floorFormRelaxedValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("FloorForm has {} errors", err.getErrorCount());
            EstablishmentAreaErrorCode ec = EstablishmentAreaErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("FloorForm error detail: {}", ec);
            throw new FloorException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of FloorForm are empty");
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, 
                    new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of FloorForm are valid");

        Optional<FloorEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of FloorForm");
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED, 
                    new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from FloorForm to FloorEntity");

        FloorEntity expectedEntity = optExpectedEntity.get();

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from FloorEntity to FloorForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = floorRepository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency account details" });
        }
        log.info("Updated existing FloorEntity with id: {}", actualEntity.getFlrId());
    }

    @Transactional
    @Override
    public void deleteFloor(String id) throws FloorException {
        log.info("Soft deleting FloorEntity by id: {}", id);

        log.debug(FloorMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_FLOOR_ENTITY_ID.getValue(), id);
        Long accountId = parseFloorId(id);
        Optional<FloorEntity> optEntity = floorRepository.findById(accountId);
        if(optEntity.isEmpty()) {
            log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_ID_INVALID.getValue(), id);
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(FloorMessageTemplate.MSG_TEMPLATE_FOUND_FLOOR_ENTITY_ID.getValue(), id);

        FloorEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("FloorEntity is inactive with id: {}", id);
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("FloorEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        FloorEntity expectedEntity = floorRepository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current account details with id:" + id });
        }

        log.info("Soft deleted existing FloorEntity with id: {}", actualEntity.getFlrId());
    }

    @Override
    public void applyPatchOnFloor(String id, List<PatchOperationForm> patches) throws FloorException {
        throw new UnsupportedOperationException("Not declared");
    }


}
