package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.service.impl;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.converter.KitchenDto2EntityConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.converter.KitchenEntity2VoConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.converter.KitchenForm2EntityConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.*;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.mapper.KitchenEntitySelfMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.mapper.KitchenForm2EntityMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.repository.KitchenRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.service.KitchenService;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.validator.KitchenFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.validator.KitchenFormValidator;
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
    
    private KitchenForm2EntityMapper form2EntityMapper;
    private KitchenEntitySelfMapper entitySelfMapper;

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
    public List<KitchenVo> retrieveListOfAllKitchens() {
        log.info("Retrieving all the KitchenEntities");
        List<KitchenEntity> kitchenEntities = kitchenRepository.findAll();
        List<KitchenVo> naturallyOrderedList = new ArrayList<>();
        for(KitchenEntity entity : kitchenEntities) {
            KitchenVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedList.add(dto);
        }
        return naturallyOrderedList.stream()
                .sorted(CMP_BY_KITCHEN_NAME)
                .collect(Collectors.toList());
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
        KitchenVo vo = entity2VoConverter.convert(entity);
        log.info("Found KitchenVo by id: {}", id);
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

    private List<KitchenVo> entity2DetailedVoList(List<KitchenEntity> kitchenEntityList) {
        List<KitchenVo> kitchenDetailsList = new ArrayList<>(kitchenEntityList.size());
        for(KitchenEntity entity : kitchenEntityList) {
            KitchenVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            kitchenDetailsList.add(vo);
        }
        return kitchenDetailsList;
    }

    @Transactional
    @Override
    public List<KitchenVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalKitchenName) throws KitchenException {

        String kitchenName = optionalKitchenName.get();
        log.info("Requesting KitchenEntity by kitchen name: {}", kitchenName);

        List<KitchenVo> matchedAccountList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        KitchenEntity entity = new KitchenEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(kitchenName))) {
            log.debug("kitchenName {} is valid", kitchenName);
            providedFilters.put("flrName", kitchenName);
            entity.setKitchenName(kitchenName);
            matcherCriteria = matcherCriteria.withMatcher("flrName", ExampleMatcher.GenericPropertyMatchers.contains());
        }
        Example<KitchenEntity> accountEntityExample = Example.of(entity, matcherCriteria);
        List<KitchenEntity> accountEntityList = kitchenRepository.findAll(accountEntityExample);
        if(accountEntityList != null && !accountEntityList.isEmpty()) {
            matchedAccountList = entity2DetailedVoList(accountEntityList);
            log.info("Found {} KitchenVo matching with provided parameters : {}", matchedAccountList.size(), providedFilters);
        } else
            log.info("Found no KitchenVo available matching with provided parameters : {}", providedFilters);
        return matchedAccountList;
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

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from KitchenEntity to KitchenForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = kitchenRepository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency account details" });
        }
        log.info("Updated existing KitchenEntity with id: {}", actualEntity.getKitchenId());
    }

    @Transactional
    @Override
    public void deleteKitchen(String id) throws KitchenException {
        log.info("Soft deleting KitchenEntity by id: {}", id);

        log.debug(KitchenMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_KITCHEN_ENTITY_ID.getValue(), id);
        Long accountId = parseKitchenId(id);
        Optional<KitchenEntity> optEntity = kitchenRepository.findById(accountId);
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
                    new Object[]{ "deletion", "unable to soft delete current account details with id:" + id });
        }

        log.info("Soft deleted existing KitchenEntity with id: {}", actualEntity.getKitchenId());
    }

    @Override
    public void applyPatchOnKitchen(String id, List<PatchOperationForm> patches) throws KitchenException {
        throw new UnsupportedOperationException("Not declared");
    }


}
