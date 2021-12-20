package com.teenthofabud.restaurant.solution.settings.charge.service.impl;

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
import com.teenthofabud.restaurant.solution.settings.charge.converter.ChargeDto2DocumentConverter;
import com.teenthofabud.restaurant.solution.settings.charge.converter.ChargeForm2DocumentConverter;
import com.teenthofabud.restaurant.solution.settings.charge.data.*;
import com.teenthofabud.restaurant.solution.settings.charge.mapper.ChargeDocumentSelfMapper;
import com.teenthofabud.restaurant.solution.settings.charge.mapper.ChargeForm2DocumentMapper;
import com.teenthofabud.restaurant.solution.settings.charge.repository.ChargeRepository;
import com.teenthofabud.restaurant.solution.settings.charge.service.ChargeService;
import com.teenthofabud.restaurant.solution.settings.charge.validator.ChargeDtoValidator;
import com.teenthofabud.restaurant.solution.settings.charge.validator.ChargeFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.settings.charge.validator.ChargeFormValidator;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import com.teenthofabud.restaurant.solution.settings.utils.SettingsServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;

@Component
@Slf4j
public class ChargeServiceImpl implements ChargeService {

    private static final Comparator<ChargeVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private ChargeForm2DocumentConverter form2DocumentConverter;
    private ChargeDto2DocumentConverter dto2DocumentConverter;
    private ChargeForm2DocumentMapper form2DocumentMapper;
    private ChargeDocumentSelfMapper entitySelfMapper;
    private ChargeFormValidator formValidator;
    private ChargeFormRelaxedValidator relaxedFormValidator;
    private ChargeDtoValidator dtoValidator;
    private ChargeRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private SettingsServiceHelper settingsServiceHelper;

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }


    @Autowired
    public void setSettingsServiceHelper(SettingsServiceHelper settingsServiceHelper) {
        this.settingsServiceHelper = settingsServiceHelper;
    }

    @Autowired
    public void setDto2DocumentConverter(ChargeDto2DocumentConverter dto2DocumentConverter) {
        this.dto2DocumentConverter = dto2DocumentConverter;
    }

    @Autowired
    public void setForm2DocumentMapper(ChargeForm2DocumentMapper form2DocumentMapper) {
        this.form2DocumentMapper = form2DocumentMapper;
    }

    @Autowired
    public void setDocumentSelfMapper(ChargeDocumentSelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(ChargeFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchChargeValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(ChargeDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2DocumentConverter(ChargeForm2DocumentConverter form2DocumentConverter) {
        this.form2DocumentConverter = form2DocumentConverter;
    }

    @Autowired
    public void setRepository(ChargeRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(ChargeFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseChargeId(String id) throws ChargeException {
        Long chargeId = null;
        try {
            chargeId = Long.parseLong(id);
            log.debug("Parsed id {} to charge id {} in numeric format", id, chargeId);
            if(chargeId <= 0) {
                throw new NumberFormatException("charge id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse charge id", e);
            log.debug(ChargeMessageTemplate.MSG_TEMPLATE_CHARGE_ID_INVALID.getValue(), id);
            throw new ChargeException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return chargeId;
    }

    @Override
    public Set<ChargeVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all ChargeDocument by their natural ordering");
        List<ChargeDocument> chargeDocumentList = repository.findAll();
        List<ChargeVo> chargeVoList = settingsServiceHelper.chargeDocument2DetailedVo(chargeDocumentList);
        Set<ChargeVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_NAME);
        naturallyOrderedSet.addAll(chargeVoList);
        log.info("{} ChargeVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public ChargeVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws ChargeException {
        log.info("Requesting ChargeDocument by id: {}", id);
        Optional<ChargeDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug("No ChargeDocument found by id: {}", id);
            throw new ChargeException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found ChargeVo by id: {}", id);
        ChargeDocument entity = optDocument.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        ChargeVo vo = settingsServiceHelper.chargeDocument2DetailedVo(entity);
        log.debug("ChargeVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<ChargeVo> retrieveAllMatchingDetailsByCriteria(
            Optional<String> optionalName, Optional<String> optionalDescription) throws ChargeException {
        if(optionalName.isEmpty() && optionalDescription.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String description = optionalDescription.isPresent() ? optionalDescription.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(description))) {
            log.debug("All search parameters are empty");
        }
        List<ChargeVo> matchedChargeList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        ChargeDocument entity = new ChargeDocument();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            log.debug("name {} is valid", name);
            providedFilters.put("name", name);
            entity.setName(name);
            matcherCriteria = matcherCriteria.withMatcher("name", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(description))) {
            log.debug("description {} is valid", description);
            providedFilters.put("description", description);
            entity.setDescription(description);
            matcherCriteria = matcherCriteria.withMatcher("description", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<ChargeDocument> chargeDocumentExample = Example.of(entity, matcherCriteria);
        List<ChargeDocument> chargeDocumentList = repository.findAll(chargeDocumentExample);
        matchedChargeList = settingsServiceHelper.chargeDocument2DetailedVo(chargeDocumentList);
        log.info("Found {} ChargeVo matching with provided parameters : {}", matchedChargeList.size(), providedFilters);
        log.info("No ChargeVo available matching with provided parameters : {}", matchedChargeList.size(), providedFilters);
        return matchedChargeList;
    }

    @Override
    public String createCharge(ChargeForm form) throws ChargeException {
        log.info("Creating new ChargeDocument");

        if(form == null) {
            log.debug("ChargeForm provided is null");
            throw new ChargeException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of ChargeForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("ChargeForm has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("ChargeForm error detail: {}", ec);
            throw new ChargeException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of ChargeForm are valid");

        ChargeDocument expectedDocument = form2DocumentConverter.convert(form);

        log.debug(ChargeMessageTemplate.MSG_TEMPLATE_CHARGE_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(repository.existsByName(expectedDocument.getName())) {
            log.debug(ChargeMessageTemplate.MSG_TEMPLATE_CHARGE_EXISTS_BY_NAME.getValue(), expectedDocument.getName());
            throw new ChargeException(SettingsErrorCode.SETTINGS_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(ChargeMessageTemplate.MSG_TEMPLATE_CHARGE_NON_EXISTENCE_BY_NAME.getValue(), expectedDocument.getName());

        log.debug("Saving {}", expectedDocument);
        ChargeDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null) {
            log.debug("Unable to create {}", expectedDocument);
            throw new ChargeException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist ChargeForm details" });
        }
        log.info("Created new ChargeForm with id: {}", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Override
    public void updateCharge(String id, ChargeForm form) throws ChargeException {
        log.info("Updating ChargeForm by id: {}", id);

        log.debug(ChargeMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHARGE_ENTITY_ID.getValue(), id);
        Optional<ChargeDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(ChargeMessageTemplate.MSG_TEMPLATE_NO_CHARGE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ChargeException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ChargeMessageTemplate.MSG_TEMPLATE_FOUND_CHARGE_ENTITY_ID.getValue(), id);

        ChargeDocument actualDocument = optActualDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("ChargeDocument is inactive with id: {}", id);
            throw new ChargeException(SettingsErrorCode.SETTINGS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("ChargeDocument is active with id: {}", id);

        if(form == null) {
            log.debug("ChargeForm is null");
            throw new ChargeException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of ChargeForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("ChargeForm has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("ChargeForm error detail: {}", ec);
            throw new ChargeException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of ChargeForm are empty");
            throw new ChargeException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of ChargeForm are valid");

        Optional<ChargeDocument> optExpectedDocument = form2DocumentMapper.compareAndMap(actualDocument, form);
        if(optExpectedDocument.isEmpty()) {
            log.debug("No new value for attributes of ChargeForm");
            throw new ChargeException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from ChargeForm to ChargeDocument");

        ChargeDocument expectedDocument = optExpectedDocument.get();

        log.debug(ChargeMessageTemplate.MSG_TEMPLATE_CHARGE_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(actualDocument.getName().compareTo(expectedDocument.getName()) == 0
                || repository.existsByName(expectedDocument.getName())) {
            log.debug(ChargeMessageTemplate.MSG_TEMPLATE_CHARGE_EXISTS_BY_NAME.getValue(), expectedDocument.getName());
            throw new ChargeException(SettingsErrorCode.SETTINGS_EXISTS,
                    new Object[]{ "name", actualDocument.getName() });
        }
        log.debug(ChargeMessageTemplate.MSG_TEMPLATE_CHARGE_NON_EXISTENCE_BY_NAME.getValue(), expectedDocument.getName());

        entitySelfMapper.compareAndMap(expectedDocument, actualDocument);
        log.debug("Compared and copied attributes from ChargeDocument to ChargeForm");
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Updated: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to update {}", actualDocument);
            throw new ChargeException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency charge details" });
        }
        log.info("Updated existing ChargeDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void deleteCharge(String id) throws ChargeException {
        log.info("Soft deleting ChargeDocument by id: {}", id);

        log.debug(ChargeMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHARGE_ENTITY_ID.getValue(), id);
        Optional<ChargeDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug(ChargeMessageTemplate.MSG_TEMPLATE_NO_CHARGE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ChargeException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ChargeMessageTemplate.MSG_TEMPLATE_FOUND_CHARGE_ENTITY_ID.getValue(), id);

        ChargeDocument actualDocument = optDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("ChargeDocument is inactive with id: {}", id);
            throw new ChargeException(SettingsErrorCode.SETTINGS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("ChargeDocument is active with id: {}", id);

        actualDocument.setActive(Boolean.FALSE);
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualDocument);
        ChargeDocument expectedDocument = repository.save(actualDocument);
        log.debug("Soft deleted: {}", expectedDocument);
        if(expectedDocument == null) {
            log.debug("Unable to soft delete {}", actualDocument);
            throw new ChargeException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current charge details with id:" + id });
        }

        log.info("Soft deleted existing ChargeDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void applyPatchOnCharge(String id, List<PatchOperationForm> patches) throws ChargeException {
        log.info("Patching ChargeDocument by id: {}", id);

        log.debug(ChargeMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHARGE_ENTITY_ID.getValue(), id);
        Optional<ChargeDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(ChargeMessageTemplate.MSG_TEMPLATE_NO_CHARGE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ChargeException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ChargeMessageTemplate.MSG_TEMPLATE_FOUND_CHARGE_ENTITY_ID.getValue(), id);

        ChargeDocument actualDocument = optActualDocument.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Charge patch list not provided");
            throw new ChargeException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Charge patch list has {} items", patches.size());


        log.debug("Validating patch list items for Charge");
        try {
            toabBaseService.validatePatches(patches, SettingsErrorCode.SETTINGS_EXISTS.getDomain() + ":LOV");
            log.debug("All Charge patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Charge patch item are invalid");
            throw new ChargeException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Charge");


        log.debug("Patching list items to ChargeDto");
        ChargeDto patchedChargeForm = new ChargeDto();
        try {
            log.debug("Preparing patch list items for Charge");
            JsonNode chargeDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch chargePatch = JsonPatch.fromJson(chargeDtoTree);
            log.debug("Prepared patch list items for Charge");
            JsonNode blankChargeDtoTree = om.convertValue(new ChargeDto(), JsonNode.class);
            JsonNode patchedChargeFormTree = chargePatch.apply(blankChargeDtoTree);
            log.debug("Applying patch list items to ChargeDto");
            patchedChargeForm = om.treeToValue(patchedChargeFormTree, ChargeDto.class);
            log.debug("Applied patch list items to ChargeDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to ChargeDto: {}", e);
            ChargeException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in ChargeDto");
                ex = new ChargeException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new ChargeException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to ChargeDto: {}", e);
            throw new ChargeException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to ChargeDto");

        log.debug("Validating patched ChargeDto");
        Errors err = new DirectFieldBindingResult(patchedChargeForm, patchedChargeForm.getClass().getSimpleName());
        dtoValidator.validate(patchedChargeForm, err);
        if(err.hasErrors()) {
            log.debug("Patched ChargeDto has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched ChargeDto error detail: {}", ec);
            throw new ChargeException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched ChargeDto are valid");

        log.debug(ChargeMessageTemplate.MSG_TEMPLATE_CHARGE_EXISTENCE_BY_NAME.getValue(), patchedChargeForm.getName().get());
        if(actualDocument.getName().compareTo(patchedChargeForm.getName().get()) == 0
                || repository.existsByName(patchedChargeForm.getName().get())) {
            log.debug(ChargeMessageTemplate.MSG_TEMPLATE_CHARGE_EXISTS_BY_NAME.getValue(), patchedChargeForm.getName().get());
            throw new ChargeException(SettingsErrorCode.SETTINGS_EXISTS,
                    new Object[]{ "name", patchedChargeForm.getName().get() });
        }
        log.debug(ChargeMessageTemplate.MSG_TEMPLATE_CHARGE_NON_EXISTENCE_BY_NAME.getValue(), patchedChargeForm.getName().get());


        log.debug("Comparatively copying patched attributes from ChargeDto to ChargeDocument");
        try {
            dto2DocumentConverter.compareAndMap(patchedChargeForm, actualDocument);
        } catch (TOABBaseException e) {
            throw (ChargeException) e;
        }
        log.debug("Comparatively copied patched attributes from ChargeDto to ChargeDocument");

        log.debug("Saving patched ChargeDocument: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Saved patched ChargeDocument: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to patch delete ChargeDocument with id:{}", id);
            throw new ChargeException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency charge details with id:" + id });
        }
        log.info("Patched ChargeDocument with id:{}", id);
    }
}