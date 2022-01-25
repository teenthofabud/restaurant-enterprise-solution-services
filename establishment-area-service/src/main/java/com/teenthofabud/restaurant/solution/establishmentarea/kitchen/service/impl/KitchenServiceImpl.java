package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.service.impl;

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
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.service.FloorService;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.converter.KitchenDto2EntityConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.converter.KitchenEntity2VoConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.converter.KitchenForm2EntityConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.*;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.mapper.KitchenEntitySelfMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.mapper.KitchenForm2EntityMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.repository.KitchenRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.service.KitchenService;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.validator.KitchenDtoValidator;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.validator.KitchenFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.validator.KitchenFormValidator;
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

@Component
@Slf4j
public class KitchenServiceImpl implements KitchenService {

    private static final Comparator<KitchenVo> CMP_BY_KITCHEN_NAME = (s1, s2) -> {
        return s1.getKitchenName().compareTo(s2.getKitchenName());
    };

    private KitchenRepository kitchenRepository;
    
    private KitchenEntity2VoConverter entity2VoConverter;
    private KitchenForm2EntityConverter form2EntityConverter;
    private KitchenDto2EntityConverter dto2EntityConverter;
    
    private KitchenFormValidator formValidator;
    private KitchenFormRelaxedValidator kitchenFormRelaxedValidator;
    private KitchenDtoValidator kitchenDtoValidator;

    private KitchenForm2EntityMapper form2EntityMapper;
    private KitchenEntitySelfMapper entitySelfMapper;

    private TOABBaseService toabBaseService;
    private ObjectMapper om;

    private FloorService floorService;
    private EstablishmentAreaServiceHelper establishmentAreaServiceHelper;

    @Autowired
    public void setKitchenDtoValidator(KitchenDtoValidator kitchenDtoValidator) {
        this.kitchenDtoValidator = kitchenDtoValidator;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setForm2EntityMapper(KitchenForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(KitchenEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setKitchenFormRelaxedValidator(KitchenFormRelaxedValidator kitchenFormRelaxedValidator) {
        this.kitchenFormRelaxedValidator = kitchenFormRelaxedValidator;
    }

    @Autowired
    public void setEntity2VoConverter(KitchenEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setForm2EntityConverter(KitchenForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setDto2EntityConverter(KitchenDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setFormValidator(KitchenFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    @Autowired
    public void setKitchenRepository(KitchenRepository kitchenRepository) {
        this.kitchenRepository = kitchenRepository;
    }

    @Autowired
    public void setFloorService(FloorService floorService) {
        this.floorService = floorService;
    }

    @Autowired
    public void setEstablishmentAreaServiceHelper(EstablishmentAreaServiceHelper establishmentAreaServiceHelper) {
        this.establishmentAreaServiceHelper = establishmentAreaServiceHelper;
    }

    @Transactional
    @Override
    public String createKitchen(KitchenForm form) throws KitchenException{
        log.info("Creating new KitchenEntity");
        log.debug("Form details: {}", form);
        log.debug("Validating provided attributes of KitchenForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("KitchenForm has {} errors", err.getErrorCount());
            EstablishmentAreaErrorCode ec = EstablishmentAreaErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("KitchenForm error detail: {}", ec);
            throw new KitchenException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of KitchenForm are valid");

        log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_EXISTENCE_BY_NAME_AND_FLOOR_ID.getValue(), form.getKitchenName(), form.getFloorId());
        if(kitchenRepository.existsByKitchenNameAndFloorFlrId(form.getKitchenName(), Long.parseLong(form.getFloorId()))) {
            log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_EXISTS_BY_NAME_AND_FLOOR_ID.getValue(), form.getKitchenName(), form.getFloorId());
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS,
                    new Object[]{ "kitchenName: " + form.getKitchenName(), "floorId: " + form.getFloorId() });
        }
        log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_NON_EXISTENCE_BY_NAME_AND_FLOOR_ID.getValue(),
                form.getKitchenName(), form.getFloorId());

        KitchenEntity expectedEntity = form2EntityConverter.convert(form);

        log.debug("Saving {}", expectedEntity);
        KitchenEntity actualEntity = kitchenRepository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE,
                    new Object[]{ "creation", "Unable to persist KitchenForm details" });
        }
        log.info("Created new KitchenForm with id: {}", actualEntity.getKitchenId());
        return actualEntity.getKitchenId().toString();
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<KitchenVo> retrieveListOfAllKitchens() throws KitchenException {
        log.info("Retrieving all the KitchenEntities");
        List<KitchenEntity> kitchenEntities = kitchenRepository.findAll();
        List<KitchenVo> naturallyOrderedList = establishmentAreaServiceHelper.kitchenEntity2DetailedVo(kitchenEntities);
        return naturallyOrderedList.stream().sorted(CMP_BY_KITCHEN_NAME).collect(Collectors.toList());
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public KitchenVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel)
            throws KitchenException {
        log.info("Requesting KitchenEntity by id: {}", id);
        Long kitchenId = parseKitchenId(id);
        Optional<KitchenEntity> optEntity = kitchenRepository.findById(kitchenId);
        if(optEntity.isEmpty()) {
            log.debug("No KitchenEntity found by id: {}", id);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND,
                    new Object[] { "id", String.valueOf(id) });
        }
        KitchenEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        KitchenVo vo = establishmentAreaServiceHelper.kitchenEntity2DetailedVo(entity);
        log.debug("KitchenVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    private Long parseKitchenId(String id) throws KitchenException {
        Long kitchenId = null;
        try {
            kitchenId = Long.parseLong(id);
            log.debug("Parsed id {} to kitchen id {} in numeric format", id, kitchenId);
            if(kitchenId <= 0) {
                throw new NumberFormatException("kitchen id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse kitchen id", e);
            log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_ID_INVALID.getValue(), id);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID,
                    new Object[] { "id", id });
        }
        return kitchenId;
    }

    /*private List<KitchenVo> entity2DetailedVoList(List<KitchenEntity> kitchenEntityList) {
        List<KitchenVo> kitchenDetailsList = new ArrayList<>(kitchenEntityList.size());
        for(KitchenEntity entity : kitchenEntityList) {
            KitchenVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            kitchenDetailsList.add(vo);
        }
        return kitchenDetailsList;
    }*/

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<KitchenVo> retrieveAllMatchingDetailsByFloorId(String floorId) throws KitchenException {
        log.info("Requesting KitchenEntity that match with floorId: {}", floorId);
        Errors err = new DirectFieldBindingResult(floorId, "KitchenForm");
        try {
            FloorVo floorVo = floorService.retrieveDetailsById(floorId, Optional.of(TOABCascadeLevel.ONE));
            if(!floorVo.getActive()) {
                throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_INACTIVE, new Object [] { floorId });
            }
        } catch (FloorException e) {
            log.error("floorId is invalid", e);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object [] { "floorId: " + floorId });
        }
        List<KitchenEntity> kitchenEntityList = kitchenRepository.findByFloorFlrId(Long.parseLong(floorId));
        if(kitchenEntityList != null && !kitchenEntityList.isEmpty()) {
            List<KitchenVo> matchedKitchenList = establishmentAreaServiceHelper.kitchenEntity2DetailedVo(kitchenEntityList);
            log.info("Found {} KitchenVo matching with floorId: {}", matchedKitchenList.size(),floorId);
            return matchedKitchenList;
        }
        log.debug("No KitchenVo found matching with floorId: {}", floorId);
        throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND, new Object[] { "floorId", floorId });
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<KitchenVo> retrieveAllMatchingDetailsByFloorId(String floorId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws KitchenException {
        log.info("Requesting KitchenEntity that match with floorId: {}", floorId);
        Errors err = new DirectFieldBindingResult(floorId, "KitchenForm");
        try {
            FloorVo floorVo = floorService.retrieveDetailsById(floorId, Optional.of(TOABCascadeLevel.ONE));
            if(!floorVo.getActive()) {
                throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_INACTIVE, new Object [] { floorId });
            }
        } catch (FloorException e) {
            log.error("floorId is invalid", e);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object [] { "floorId: " + floorId });
        }
        List<KitchenEntity> kitchenEntityList = kitchenRepository.findByFloorFlrId(Long.parseLong(floorId));
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        List<KitchenVo> matchedKitchenList = establishmentAreaServiceHelper.kitchenEntity2DetailedVo(kitchenEntityList);
        log.info("Found {} KitchenVo matching with floorId: {}", matchedKitchenList.size(),floorId);
        TOABRequestContextHolder.clearCascadeLevelContext();
        if(matchedKitchenList.isEmpty()) {
            log.debug("No KitchenVo found matching with floorId: {}", floorId);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND, new Object[] { "floorId", floorId });
        }
        return matchedKitchenList;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<KitchenVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalKitchenName,
                                                                Optional<String> optionalDescription) throws KitchenException {

        String kitchenName = optionalKitchenName.isPresent() ? optionalKitchenName.get() : "";
        String description = optionalDescription.isPresent() ? optionalDescription.get() : "";

        List<KitchenVo> matchedFloorList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        KitchenEntity entity = new KitchenEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(kitchenName))) {
            log.debug("kitchenName {} is valid", kitchenName);
            providedFilters.put("kitchenName", kitchenName);
            entity.setKitchenName(kitchenName);
            matcherCriteria = matcherCriteria.withMatcher("kitchenName", ExampleMatcher.GenericPropertyMatchers.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(description))) {
            log.debug("description {} is valid", description);
            providedFilters.put("description", description);
            entity.setDescription(description);
            matcherCriteria = matcherCriteria.withMatcher("description", ExampleMatcher.GenericPropertyMatchers.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<KitchenEntity> floorEntityExample = Example.of(entity, matcherCriteria);
        List<KitchenEntity> floorEntityList = kitchenRepository.findAll(floorEntityExample);
        if(floorEntityList != null && !floorEntityList.isEmpty()) {
            matchedFloorList = establishmentAreaServiceHelper.kitchenEntity2DetailedVo(floorEntityList);
            log.info("Found {} KitchenVo matching with provided parameters : {}", matchedFloorList.size(), providedFilters);
        } else
            log.info("Found no KitchenVo available matching with provided parameters : {}", providedFilters);
        return matchedFloorList;
    }

    @Transactional
    @Override
    public void updateKitchen(String id, KitchenForm form) throws KitchenException {
        log.info("Updating KitchenEntity by id: {}", id);
        Long kitchenId = parseKitchenId(id);
        Optional<KitchenEntity> optEntity = kitchenRepository.findById(kitchenId);
        if(optEntity.isEmpty()) {
            log.debug("No KitchenEntity found by id: {}", id);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND,
                    new Object[] { "id", String.valueOf(id) });
        }
        log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_ID_VALID.getValue(), id);

        KitchenEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("KitchenEntity is inactive with id: {}", id);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("KitchenEntity is active with id: {}", id);

        if(form == null) {
            log.debug("KitchenForm is null");
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED, 
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of KitchenForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = kitchenFormRelaxedValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("KitchenForm has {} errors", err.getErrorCount());
            EstablishmentAreaErrorCode ec = EstablishmentAreaErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("KitchenForm error detail: {}", ec);
            throw new KitchenException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of KitchenForm are empty");
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, 
                    new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of KitchenForm are valid");

        Optional<KitchenEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of KitchenForm");
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED, 
                    new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from KitchenForm to KitchenEntity");

        KitchenEntity expectedEntity = optExpectedEntity.get();

        checkUniquenessOfKitchen(form, actualEntity);

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from KitchenEntity to KitchenForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = kitchenRepository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency floor details" });
        }
        log.info("Updated existing KitchenEntity with id: {}", actualEntity.getKitchenId());
    }

    @Transactional
    @Override
    public void deleteKitchen(String id) throws KitchenException {
        log.info("Soft deleting KitchenEntity by id: {}", id);

        log.debug(KitchenMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_KITCHEN_ENTITY_ID.getValue(), id);
        Long floorId = parseKitchenId(id);
        Optional<KitchenEntity> optEntity = kitchenRepository.findById(floorId);
        if(optEntity.isEmpty()) {
            log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_ID_INVALID.getValue(), id);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(KitchenMessageTemplate.MSG_TEMPLATE_FOUND_KITCHEN_ENTITY_ID.getValue(), id);

        KitchenEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("KitchenEntity is inactive with id: {}", id);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("KitchenEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        KitchenEntity expectedEntity = kitchenRepository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current floor details with id:" + id });
        }

        log.info("Soft deleted existing KitchenEntity with id: {}", actualEntity.getKitchenId());
    }

    @Transactional
    @Override
    public void applyPatchOnKitchen(String id, List<PatchOperationForm> patches) throws KitchenException {
        log.info("Patching KitchenEntity by id: {}", id);

        log.debug(KitchenMessageTemplate.MSG_TEMPLATE_FOUND_KITCHEN_ENTITY_ID.getValue(), id);
        Long kitchenId = parseKitchenId(id);
        Optional<KitchenEntity> optActualEntity = kitchenRepository.findById(kitchenId);
        if(optActualEntity.isEmpty()) {
            log.debug(KitchenMessageTemplate.MSG_TEMPLATE_FOUND_KITCHEN_ENTITY_ID.getValue(), id);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(KitchenMessageTemplate.MSG_TEMPLATE_FOUND_KITCHEN_ENTITY_ID.getValue(), id);

        KitchenEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Kitchen patch list not provided");
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Kitchen patch list has {} items", patches.size());
        log.debug("Validating patch list items for Kitchen");
        try {
            toabBaseService.validatePatches(patches, EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS.getDomain() + ":LOV");
            log.debug("All Kitchen patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Kitchen patch item are invalid");
            throw new KitchenException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Kitchen");
        log.debug("Patching list items to KitchenDto");
        KitchenDto patchedKitchenForm = new KitchenDto();
        try {
            log.debug("Preparing patch list items for Kitchen");
            JsonNode kitchenDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch kitchenPatch = JsonPatch.fromJson(kitchenDtoTree);
            log.debug("Prepared patch list items for Kitchen");
            log.debug("Applying patch list items to KitchenDto");
            JsonNode blankKitchenDtoTree = om.convertValue(new KitchenDto(), JsonNode.class);
            JsonNode patchedKitchenFormTree = kitchenPatch.apply(blankKitchenDtoTree);
            patchedKitchenForm = om.treeToValue(patchedKitchenFormTree, KitchenDto.class);
            log.debug("Applied patch list items to KitchenDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to KitchenDto: {}", e);
            KitchenException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in KitchenDto");
                ex = new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to KitchenDto: {}", e);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to KitchenDto");

        log.debug("Validating patched KitchenDto");
        Errors err = new DirectFieldBindingResult(patchedKitchenForm, patchedKitchenForm.getClass().getSimpleName());
        kitchenDtoValidator.validate(patchedKitchenForm, err);
        if(err.hasErrors()) {
            log.debug("Patched KitchenDto has {} errors", err.getErrorCount());
            EstablishmentAreaErrorCode ec = EstablishmentAreaErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched KitchenDto error detail: {}", ec);
            throw new KitchenException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched KitchenDto are valid");

        checkUniquenessOfKitchen(patchedKitchenForm, actualEntity);

        log.debug("Comparatively copying patched attributes from KitchenDto to KitchenEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedKitchenForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (KitchenException) e;
        }
        log.debug("Comparatively copied patched attributes from KitchenDto to KitchenEntity");

        log.debug("Saving patched KitchenEntity: {}", actualEntity);
        actualEntity = kitchenRepository.save(actualEntity);
        log.debug("Saved patched KitchenEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete KitchenEntity with id:{}", id);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch kitchen details with id:" + id });
        }
        log.info("Patched KitchenEntity with id:{}", id);
    }

    private void checkUniquenessOfKitchen(KitchenForm kitchenForm, KitchenEntity actualEntity) throws KitchenException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(2);
        if(StringUtils.hasText(StringUtils.trimWhitespace(kitchenForm.getKitchenName()))) {
            similaritySwitchesCollection.add(kitchenForm.getKitchenName().compareTo(actualEntity.getKitchenName()) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(kitchenForm.getFloorId()))) {
            similaritySwitchesCollection.add(kitchenForm.getFloorId().compareTo(actualEntity.getFloor().getFlrId().toString()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String kitchenName = StringUtils.hasText(StringUtils.trimWhitespace(kitchenForm.getKitchenName())) ? kitchenForm.getKitchenName() : actualEntity.getKitchenName();
            String floorId = StringUtils.hasText(StringUtils.trimWhitespace(kitchenForm.getFloorId())) ?
                    kitchenForm.getFloorId() : actualEntity.getFloor().getFlrId().toString();
            log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_EXISTENCE_BY_NAME_AND_FLOOR_ID.getValue(), kitchenName, floorId);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  kitchenRepository.existsByKitchenNameAndFloorFlrId(kitchenName, Long.parseLong(floorId));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_EXISTS_BY_NAME_AND_FLOOR_ID.getValue(), kitchenName, floorId);
                throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS, new Object[]{ "kitchenName: " + kitchenName, ", floorId: " + floorId});
            }
            log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_NON_EXISTENCE_BY_NAME_AND_FLOOR_ID.getValue(), kitchenName, floorId);

        }
    }

    private void checkUniquenessOfKitchen(KitchenDto patchedKitchenForm, KitchenEntity actualEntity) throws KitchenException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(2);
        if(patchedKitchenForm.getKitchenName().isPresent()) {
            similaritySwitchesCollection.add(patchedKitchenForm.getKitchenName().get().compareTo(actualEntity.getKitchenName()) == 0);
        }
        if(patchedKitchenForm.getFloorId().isPresent()) {
            similaritySwitchesCollection.add(patchedKitchenForm.getFloorId().get().compareTo(actualEntity.getFloor().getFlrId().toString()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String kitchenName = patchedKitchenForm.getKitchenName().isPresent() ? patchedKitchenForm.getKitchenName().get() : actualEntity.getKitchenName();
            String floorId = patchedKitchenForm.getFloorId().isPresent() ? patchedKitchenForm.getFloorId().get() : actualEntity.getFloor().getFlrId().toString();
            log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_EXISTENCE_BY_NAME_AND_FLOOR_ID.getValue(), kitchenName, floorId);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  kitchenRepository.existsByKitchenNameAndFloorFlrId(kitchenName, Long.parseLong(floorId));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_EXISTS_BY_NAME_AND_FLOOR_ID.getValue(), kitchenName, floorId);
                throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS, new Object[]{ "kitchenName: " + kitchenName, ", floorId: " + floorId });
            }
            log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_NON_EXISTENCE_BY_NAME_AND_FLOOR_ID.getValue(), kitchenName, floorId);

        }
    }
}
