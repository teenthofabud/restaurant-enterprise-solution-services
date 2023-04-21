package com.teenthofabud.restaurant.solution.settings.discount.service.impl;

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
import com.teenthofabud.restaurant.solution.settings.discount.converter.DiscountDto2DocumentConverter;
import com.teenthofabud.restaurant.solution.settings.discount.converter.DiscountForm2DocumentConverter;
import com.teenthofabud.restaurant.solution.settings.discount.data.*;
import com.teenthofabud.restaurant.solution.settings.discount.mapper.DiscountDocumentSelfMapper;
import com.teenthofabud.restaurant.solution.settings.discount.mapper.DiscountForm2DocumentMapper;
import com.teenthofabud.restaurant.solution.settings.discount.repository.DiscountRepository;
import com.teenthofabud.restaurant.solution.settings.discount.service.DiscountService;
import com.teenthofabud.restaurant.solution.settings.discount.validator.DiscountDtoValidator;
import com.teenthofabud.restaurant.solution.settings.discount.validator.DiscountFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.settings.discount.validator.DiscountFormValidator;
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
public class DiscountServiceImpl implements DiscountService {

    private static final Comparator<DiscountVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private DiscountForm2DocumentConverter form2DocumentConverter;
    private DiscountDto2DocumentConverter dto2DocumentConverter;
    private DiscountForm2DocumentMapper form2DocumentMapper;
    private DiscountDocumentSelfMapper entitySelfMapper;
    private DiscountFormValidator formValidator;
    private DiscountFormRelaxedValidator relaxedFormValidator;
    private DiscountDtoValidator dtoValidator;
    private DiscountRepository repository;
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
    public void setDto2DocumentConverter(DiscountDto2DocumentConverter dto2DocumentConverter) {
        this.dto2DocumentConverter = dto2DocumentConverter;
    }

    @Autowired
    public void setForm2DocumentMapper(DiscountForm2DocumentMapper form2DocumentMapper) {
        this.form2DocumentMapper = form2DocumentMapper;
    }

    @Autowired
    public void setDocumentSelfMapper(DiscountDocumentSelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(DiscountFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchDiscountValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(DiscountDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2DocumentConverter(DiscountForm2DocumentConverter form2DocumentConverter) {
        this.form2DocumentConverter = form2DocumentConverter;
    }

    @Autowired
    public void setRepository(DiscountRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(DiscountFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseDiscountId(String id) throws DiscountException {
        Long discountId = null;
        try {
            discountId = Long.parseLong(id);
            log.debug("Parsed id {} to discount id {} in numeric format", id, discountId);
            if(discountId <= 0) {
                throw new NumberFormatException("discount id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse discount id", e);
            log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_ID_INVALID.getValue(), id);
            throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return discountId;
    }

    @Override
    public Set<DiscountVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all DiscountDocument by their natural ordering");
        List<DiscountDocument> engagementDocumentList = repository.findAll();
        List<DiscountVo> discountVoList = settingsServiceHelper.discountDocument2DetailedVo(engagementDocumentList);
        Set<DiscountVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_NAME);
        naturallyOrderedSet.addAll(discountVoList);
        log.info("{} DiscountVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public DiscountVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws DiscountException {
        log.info("Requesting DiscountDocument by id: {}", id);
        Optional<DiscountDocument> optDocument = repository.findById(id);
        if(!optDocument.isPresent()) {
            log.debug("No DiscountDocument found by id: {}", id);
            throw new DiscountException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found DiscountVo by id: {}", id);
        DiscountDocument entity = optDocument.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        DiscountVo vo = settingsServiceHelper.discountDocument2DetailedVo(entity);
        log.debug("DiscountVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<DiscountVo> retrieveAllMatchingDetailsByCriteria(
            Optional<String> optionalName, Optional<String> optionalDescription) throws DiscountException {
        if(!optionalName.isPresent() && !optionalDescription.isPresent()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String description = optionalDescription.isPresent() ? optionalDescription.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(description))) {
            log.debug("All search parameters are empty");
        }
        List<DiscountVo> matchedDiscountList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        DiscountDocument entity = new DiscountDocument();
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
        Example<DiscountDocument> discountDocumentExample = Example.of(entity, matcherCriteria);
        List<DiscountDocument> engagementDocumentList = repository.findAll(discountDocumentExample);
        matchedDiscountList = settingsServiceHelper.discountDocument2DetailedVo(engagementDocumentList);
        log.info("Found {} DiscountVo matching with provided parameters : {}", matchedDiscountList.size(), providedFilters);
        log.info("No DiscountVo available matching with provided parameters : {}", matchedDiscountList.size(), providedFilters);
        return matchedDiscountList;
    }

    @Override
    public String createDiscount(DiscountForm form) throws DiscountException {
        log.info("Creating new DiscountDocument");

        if(form == null) {
            log.debug("DiscountForm provided is null");
            throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of DiscountForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("DiscountForm has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("DiscountForm error detail: {}", ec);
            throw new DiscountException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of DiscountForm are valid");

        DiscountDocument expectedDocument = form2DocumentConverter.convert(form);

        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(repository.existsByName(expectedDocument.getName())) {
            log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_EXISTS_BY_NAME.getValue(), expectedDocument.getName());
            throw new DiscountException(SettingsErrorCode.SETTINGS_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_NON_EXISTENCE_BY_NAME.getValue(), expectedDocument.getName());

        log.debug("Saving {}", expectedDocument);
        DiscountDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null) {
            log.debug("Unable to create {}", expectedDocument);
            throw new DiscountException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist DiscountForm details" });
        }
        log.info("Created new DiscountForm with id: {}", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Override
    public void updateDiscount(String id, DiscountForm form) throws DiscountException {
        log.info("Updating DiscountForm by id: {}", id);

        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_DISCOUNT_ENTITY_ID.getValue(), id);
        Optional<DiscountDocument> optActualDocument = repository.findById(id);
        if(!optActualDocument.isPresent()) {
            log.debug(DiscountMessageTemplate.MSG_TEMPLATE_NO_DISCOUNT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new DiscountException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_FOUND_DISCOUNT_ENTITY_ID.getValue(), id);

        DiscountDocument actualDocument = optActualDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("DiscountDocument is inactive with id: {}", id);
            throw new DiscountException(SettingsErrorCode.SETTINGS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("DiscountDocument is active with id: {}", id);

        if(form == null) {
            log.debug("DiscountForm is null");
            throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of DiscountForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("DiscountForm has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("DiscountForm error detail: {}", ec);
            throw new DiscountException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of DiscountForm are empty");
            throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of DiscountForm are valid");

        Optional<DiscountDocument> optExpectedDocument = form2DocumentMapper.compareAndMap(actualDocument, form);
        if(!optExpectedDocument.isPresent()) {
            log.debug("No new value for attributes of DiscountForm");
            throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from DiscountForm to DiscountDocument");

        DiscountDocument expectedDocument = optExpectedDocument.get();

        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(actualDocument.getName().compareTo(expectedDocument.getName()) == 0
                || repository.existsByName(expectedDocument.getName())) {
            log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_EXISTS_BY_NAME.getValue(), expectedDocument.getName());
            throw new DiscountException(SettingsErrorCode.SETTINGS_EXISTS,
                    new Object[]{ "name", actualDocument.getName() });
        }
        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_NON_EXISTENCE_BY_NAME.getValue(), expectedDocument.getName());

        entitySelfMapper.compareAndMap(expectedDocument, actualDocument);
        log.debug("Compared and copied attributes from DiscountDocument to DiscountForm");
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Updated: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to update {}", actualDocument);
            throw new DiscountException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency discount details" });
        }
        log.info("Updated existing DiscountDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void deleteDiscount(String id) throws DiscountException {
        log.info("Soft deleting DiscountDocument by id: {}", id);

        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_DISCOUNT_ENTITY_ID.getValue(), id);
        Optional<DiscountDocument> optDocument = repository.findById(id);
        if(!optDocument.isPresent()) {
            log.debug(DiscountMessageTemplate.MSG_TEMPLATE_NO_DISCOUNT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new DiscountException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_FOUND_DISCOUNT_ENTITY_ID.getValue(), id);

        DiscountDocument actualDocument = optDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("DiscountDocument is inactive with id: {}", id);
            throw new DiscountException(SettingsErrorCode.SETTINGS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("DiscountDocument is active with id: {}", id);

        actualDocument.setActive(Boolean.FALSE);
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualDocument);
        DiscountDocument expectedDocument = repository.save(actualDocument);
        log.debug("Soft deleted: {}", expectedDocument);
        if(expectedDocument == null) {
            log.debug("Unable to soft delete {}", actualDocument);
            throw new DiscountException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current discount details with id:" + id });
        }

        log.info("Soft deleted existing DiscountDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void applyPatchOnDiscount(String id, List<PatchOperationForm> patches) throws DiscountException {
        log.info("Patching DiscountDocument by id: {}", id);

        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_DISCOUNT_ENTITY_ID.getValue(), id);
        Optional<DiscountDocument> optActualDocument = repository.findById(id);
        if(!optActualDocument.isPresent()) {
            log.debug(DiscountMessageTemplate.MSG_TEMPLATE_NO_DISCOUNT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new DiscountException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_FOUND_DISCOUNT_ENTITY_ID.getValue(), id);

        DiscountDocument actualDocument = optActualDocument.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Discount patch list not provided");
            throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Discount patch list has {} items", patches.size());


        log.debug("Validating patch list items for Discount");
        try {
            toabBaseService.validatePatches(patches, SettingsErrorCode.SETTINGS_EXISTS.getDomain() + ":LOV");
            log.debug("All Discount patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Discount patch item are invalid");
            throw new DiscountException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Discount");


        log.debug("Patching list items to DiscountDto");
        DiscountDto patchedDiscountForm = new DiscountDto();
        try {
            log.debug("Preparing patch list items for Discount");
            JsonNode discountDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch discountPatch = JsonPatch.fromJson(discountDtoTree);
            log.debug("Prepared patch list items for Discount");
            JsonNode blankDiscountDtoTree = om.convertValue(new DiscountDto(), JsonNode.class);
            JsonNode patchedDiscountFormTree = discountPatch.apply(blankDiscountDtoTree);
            log.debug("Applying patch list items to DiscountDto");
            patchedDiscountForm = om.treeToValue(patchedDiscountFormTree, DiscountDto.class);
            log.debug("Applied patch list items to DiscountDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to DiscountDto: {}", e);
            DiscountException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in DiscountDto");
                ex = new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new DiscountException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to DiscountDto: {}", e);
            throw new DiscountException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to DiscountDto");

        log.debug("Validating patched DiscountDto");
        Errors err = new DirectFieldBindingResult(patchedDiscountForm, patchedDiscountForm.getClass().getSimpleName());
        dtoValidator.validate(patchedDiscountForm, err);
        if(err.hasErrors()) {
            log.debug("Patched DiscountDto has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched DiscountDto error detail: {}", ec);
            throw new DiscountException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched DiscountDto are valid");

        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_EXISTENCE_BY_NAME.getValue(), patchedDiscountForm.getName().get());
        if(actualDocument.getName().compareTo(patchedDiscountForm.getName().get()) == 0
                || repository.existsByName(patchedDiscountForm.getName().get())) {
            log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_EXISTS_BY_NAME.getValue(), patchedDiscountForm.getName().get());
            throw new DiscountException(SettingsErrorCode.SETTINGS_EXISTS,
                    new Object[]{ "name", patchedDiscountForm.getName().get() });
        }
        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_NON_EXISTENCE_BY_NAME.getValue(), patchedDiscountForm.getName().get());


        log.debug("Comparatively copying patched attributes from DiscountDto to DiscountDocument");
        try {
            dto2DocumentConverter.compareAndMap(patchedDiscountForm, actualDocument);
        } catch (TOABBaseException e) {
            throw (DiscountException) e;
        }
        log.debug("Comparatively copied patched attributes from DiscountDto to DiscountDocument");

        log.debug("Saving patched DiscountDocument: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Saved patched DiscountDocument: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to patch delete DiscountDocument with id:{}", id);
            throw new DiscountException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency discount details with id:" + id });
        }
        log.info("Patched DiscountDocument with id:{}", id);
    }
}