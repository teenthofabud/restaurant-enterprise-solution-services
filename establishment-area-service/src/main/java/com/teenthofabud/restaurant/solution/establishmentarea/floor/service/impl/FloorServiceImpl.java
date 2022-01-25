package com.teenthofabud.restaurant.solution.establishmentarea.floor.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.fge.jsonpatch.JsonPatch;
import com.github.fge.jsonpatch.JsonPatchException;
import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.converter.FloorDto2EntityConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.converter.FloorEntity2VoConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.converter.FloorForm2EntityConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.*;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.mapper.FloorEntitySelfMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.mapper.FloorForm2EntityMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.repository.FloorRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.service.FloorService;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.validator.FloorDtoValidator;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.validator.FloorFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.validator.FloorFormValidator;
import com.teenthofabud.restaurant.solution.establishmentarea.utils.EstablishmentAreaServiceHelper;
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

import java.io.IOException;
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
    private FloorDtoValidator floorDtoValidator;
    
    private FloorForm2EntityMapper form2EntityMapper;
    private FloorEntitySelfMapper entitySelfMapper;
    private EstablishmentAreaServiceHelper establishmentAreaServiceHelper;

    private TOABBaseService toabBaseService;
    private ObjectMapper om;

    @Autowired
    public void setFloorDtoValidator(FloorDtoValidator floorDtoValidator) {
        this.floorDtoValidator = floorDtoValidator;
    }

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

    @Autowired
    public void setEstablishmentAreaServiceHelper(EstablishmentAreaServiceHelper establishmentAreaServiceHelper) {
        this.establishmentAreaServiceHelper = establishmentAreaServiceHelper;
    }
    
    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
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
        log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_EXISTENCE_BY_NAME.getValue(), form.getFlrName());
        if(floorRepository.existsByFlrName(expectedEntity.getFlrName())) {
            log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_EXISTS_BY_NAME.getValue(), expectedEntity.getFlrName());
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS,
                    new Object[]{ "flrName", form.getFlrName() });
        }
        log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getFlrName());

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
    public List<FloorVo> retrieveListOfAllFloors() throws FloorException {
        log.info("Retrieving all the FloorEntities");
        List<FloorEntity> floorEntities = floorRepository.findAll();
        List<FloorVo> naturallyOrderedList = establishmentAreaServiceHelper.floorEntity2DetailedVo(floorEntities);
                //new ArrayList<>();
        /*for(FloorEntity entity : floorEntities) {
            FloorVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedList.add(dto);
        }*/

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
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        FloorVo vo = establishmentAreaServiceHelper.floorEntity2DetailedVo(entity);
        log.debug("FloorVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
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

    /*private List<FloorVo> entity2DetailedVoList(List<FloorEntity> floorEntityList) {
        List<FloorVo> floorDetailsList = new ArrayList<>(floorEntityList.size());
        for(FloorEntity entity : floorEntityList) {
            FloorVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            floorDetailsList.add(vo);
        }
        return floorDetailsList;
    }*/

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<FloorVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalFloorName) throws FloorException {

        String floorName = optionalFloorName.isPresent() ? optionalFloorName.get() : "";
        log.info("Requesting FloorEntity by floor name: {}", floorName);

        List<FloorVo> matchedFloorList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        FloorEntity entity = new FloorEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(floorName))) {
            log.debug("floorName {} is valid", floorName);
            providedFilters.put("flrName", floorName);
            entity.setFlrName(floorName);
            matcherCriteria = matcherCriteria.withMatcher("flrName", ExampleMatcher.GenericPropertyMatchers.contains());
        }
        Example<FloorEntity> floorEntityExample = Example.of(entity, matcherCriteria);
        List<FloorEntity> floorEntityList = floorRepository.findAll(floorEntityExample);
        matchedFloorList = establishmentAreaServiceHelper.floorEntity2DetailedVo(floorEntityList);
        log.info("Found {} FloorVo matching with provided parameters : {}", matchedFloorList.size(), providedFilters);
        /*} else
            log.info("Found no FloorVo available matching with provided parameters : {}", providedFilters);*/
        return matchedFloorList;
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

        log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_EXISTENCE_BY_NAME.getValue(), form.getFlrName());
        if(floorRepository.existsByFlrName(expectedEntity.getFlrName())) {
            log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_EXISTS_BY_NAME.getValue(), expectedEntity.getFlrName());
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS,
                    new Object[]{ "flrName", form.getFlrName() });
        }
        log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getFlrName());

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from FloorEntity to FloorForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = floorRepository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency floor details" });
        }
        log.info("Updated existing FloorEntity with id: {}", actualEntity.getFlrId());
    }

    @Transactional
    @Override
    public void deleteFloor(String id) throws FloorException {
        log.info("Soft deleting FloorEntity by id: {}", id);

        log.debug(FloorMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_FLOOR_ENTITY_ID.getValue(), id);
        Long floorId = parseFloorId(id);
        Optional<FloorEntity> optEntity = floorRepository.findById(floorId);
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
                    new Object[]{ "deletion", "unable to soft delete current floor details with id:" + id });
        }

        log.info("Soft deleted existing FloorEntity with id: {}", actualEntity.getFlrId());
    }

    @Transactional
    @Override
    public void applyPatchOnFloor(String id, List<PatchOperationForm> patches) throws FloorException {
        log.info("Patching FloorEntity by id: {}", id);

        log.debug(FloorMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_FLOOR_ENTITY_ID.getValue(), id);
        Long floorId = parseFloorId(id);
        Optional<FloorEntity> optActualEntity = floorRepository.findById(floorId);
        if(optActualEntity.isEmpty()) {
            log.debug(FloorMessageTemplate.MSG_TEMPLATE_NO_FLOOR_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(FloorMessageTemplate.MSG_TEMPLATE_FOUND_FLOOR_ENTITY_ID.getValue(), id);

        FloorEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Floor patch list not provided");
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Floor patch list has {} items", patches.size());
        log.debug("Validating patch list items for Floor");
        try {
            toabBaseService.validatePatches(patches, EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS.getDomain() + ":LOV");
            log.debug("All Floor patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Floor patch item are invalid");
            throw new FloorException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Floor");
        log.debug("Patching list items to FloorDto");
        FloorDto patchedFloorForm = new FloorDto();
        try {
            log.debug("Preparing patch list items for Floor");
            JsonNode floorDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch floorPatch = JsonPatch.fromJson(floorDtoTree);
            log.debug("Prepared patch list items for Floor");
            log.debug("Applying patch list items to FloorDto");
            JsonNode blankFloorDtoTree = om.convertValue(new FloorDto(), JsonNode.class);
            JsonNode patchedFloorFormTree = floorPatch.apply(blankFloorDtoTree);
            patchedFloorForm = om.treeToValue(patchedFloorFormTree, FloorDto.class);
            log.debug("Applied patch list items to FloorDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to FloorDto: {}", e);
            FloorException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in FloorDto");
                ex = new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to FloorDto: {}", e);
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to FloorDto");

        log.debug("Validating patched FloorDto");
        Errors err = new DirectFieldBindingResult(patchedFloorForm, patchedFloorForm.getClass().getSimpleName());
        floorDtoValidator.validate(patchedFloorForm, err);
        if(err.hasErrors()) {
            log.debug("Patched FloorDto has {} errors", err.getErrorCount());
            EstablishmentAreaErrorCode ec = EstablishmentAreaErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched FloorDto error detail: {}", ec);
            throw new FloorException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched FloorDto are valid");

        log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_EXISTENCE_BY_NAME.getValue(), patchedFloorForm.getFlrName());
        if(floorRepository.existsByFlrName(patchedFloorForm.getFlrName().get())) {
            log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_EXISTS_BY_NAME.getValue(), patchedFloorForm.getFlrName());
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS,
                    new Object[]{ "flrName", patchedFloorForm.getFlrName().get() });
        }
        log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_NON_EXISTENCE_BY_NAME.getValue(), patchedFloorForm.getFlrName());

        log.debug("Comparatively copying patched attributes from FloorDto to FloorEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedFloorForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (FloorException) e;
        }
        log.debug("Comparatively copied patched attributes from FloorDto to FloorEntity");

        log.debug("Saving patched FloorEntity: {}", actualEntity);
        actualEntity = floorRepository.save(actualEntity);
        log.debug("Saved patched FloorEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete FloorEntity with id:{}", id);
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch floor details with id:" + id });
        }
        log.info("Patched FloorEntity with id:{}", id);
    }


}
