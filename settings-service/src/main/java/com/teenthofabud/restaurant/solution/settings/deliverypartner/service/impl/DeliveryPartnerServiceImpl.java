package com.teenthofabud.restaurant.solution.settings.deliverypartner.service.impl;

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
import com.teenthofabud.restaurant.solution.settings.deliverypartner.converter.DeliveryPartnerDto2DocumentConverter;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.converter.DeliveryPartnerForm2DocumentConverter;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.*;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.mapper.DeliveryPartnerDocumentSelfMapper;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.mapper.DeliveryPartnerForm2DocumentMapper;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.repository.DeliveryPartnerRepository;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.service.DeliveryPartnerService;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.validator.DeliveryPartnerDtoValidator;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.validator.DeliveryPartnerFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.validator.DeliveryPartnerFormValidator;
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
public class DeliveryPartnerServiceImpl implements DeliveryPartnerService {

    private static final Comparator<DeliveryPartnerVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private DeliveryPartnerForm2DocumentConverter form2DocumentConverter;
    private DeliveryPartnerDto2DocumentConverter dto2DocumentConverter;
    private DeliveryPartnerForm2DocumentMapper form2DocumentMapper;
    private DeliveryPartnerDocumentSelfMapper entitySelfMapper;
    private DeliveryPartnerFormValidator formValidator;
    private DeliveryPartnerFormRelaxedValidator relaxedFormValidator;
    private DeliveryPartnerDtoValidator dtoValidator;
    private DeliveryPartnerRepository repository;
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
    public void setDto2DocumentConverter(DeliveryPartnerDto2DocumentConverter dto2DocumentConverter) {
        this.dto2DocumentConverter = dto2DocumentConverter;
    }

    @Autowired
    public void setForm2DocumentMapper(DeliveryPartnerForm2DocumentMapper form2DocumentMapper) {
        this.form2DocumentMapper = form2DocumentMapper;
    }

    @Autowired
    public void setDocumentSelfMapper(DeliveryPartnerDocumentSelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(DeliveryPartnerFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchDeliveryPartnerValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(DeliveryPartnerDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2DocumentConverter(DeliveryPartnerForm2DocumentConverter form2DocumentConverter) {
        this.form2DocumentConverter = form2DocumentConverter;
    }

    @Autowired
    public void setRepository(DeliveryPartnerRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(DeliveryPartnerFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    @Override
    public Set<DeliveryPartnerVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all DeliveryPartnerDocument by their natural ordering");
        List<DeliveryPartnerDocument> deliveryPartnerDocumentList = repository.findAll();
        List<DeliveryPartnerVo> deliveryPartnerVoList = settingsServiceHelper.deliveryPartnerDocument2DetailedVo(deliveryPartnerDocumentList);
        Set<DeliveryPartnerVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_NAME);
        naturallyOrderedSet.addAll(deliveryPartnerVoList);
        log.info("{} DeliveryPartnerVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public DeliveryPartnerVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws DeliveryPartnerException {
        log.info("Requesting DeliveryPartnerDocument by id: {}", id);
        Optional<DeliveryPartnerDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug("No DeliveryPartnerDocument found by id: {}", id);
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found DeliveryPartnerVo by id: {}", id);
        DeliveryPartnerDocument entity = optDocument.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        DeliveryPartnerVo vo = settingsServiceHelper.deliveryPartnerDocument2DetailedVo(entity);
        log.debug("DeliveryPartnerVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<DeliveryPartnerVo> retrieveAllMatchingDetailsByCriteria(
            Optional<String> optionalName, Optional<String> optionalDescription) throws DeliveryPartnerException {
        if(optionalName.isEmpty() && optionalDescription.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String description = optionalDescription.isPresent() ? optionalDescription.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(description))) {
            log.debug("All search parameters are empty");
        }
        List<DeliveryPartnerVo> matchedDeliveryPartnerList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        DeliveryPartnerDocument entity = new DeliveryPartnerDocument();
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
        Example<DeliveryPartnerDocument> deliveryPartnerDocumentExample = Example.of(entity, matcherCriteria);
        List<DeliveryPartnerDocument> deliveryPartnerDocumentList = repository.findAll(deliveryPartnerDocumentExample);
        matchedDeliveryPartnerList = settingsServiceHelper.deliveryPartnerDocument2DetailedVo(deliveryPartnerDocumentList);
        log.info("Found {} DeliveryPartnerVo matching with provided parameters : {}", matchedDeliveryPartnerList.size(), providedFilters);
        log.info("No DeliveryPartnerVo available matching with provided parameters : {}", matchedDeliveryPartnerList.size(), providedFilters);
        return matchedDeliveryPartnerList;
    }

    @Override
    public String createDeliveryPartner(DeliveryPartnerForm form) throws DeliveryPartnerException {
        log.info("Creating new DeliveryPartnerDocument");

        if(form == null) {
            log.debug("DeliveryPartnerForm provided is null");
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of DeliveryPartnerForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("DeliveryPartnerForm has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("DeliveryPartnerForm error detail: {}", ec);
            throw new DeliveryPartnerException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of DeliveryPartnerForm are valid");

        DeliveryPartnerDocument expectedDocument = form2DocumentConverter.convert(form);

        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_DELIVERY_PARTNER_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(repository.existsByName(expectedDocument.getName())) {
            log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_DELIVERY_PARTNER_EXISTS_BY_NAME.getValue(), expectedDocument.getName());
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_DELIVERY_PARTNER_NON_EXISTENCE_BY_NAME.getValue(), expectedDocument.getName());

        log.debug("Saving {}", expectedDocument);
        DeliveryPartnerDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null) {
            log.debug("Unable to create {}", expectedDocument);
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist DeliveryPartnerForm details" });
        }
        log.info("Created new DeliveryPartnerForm with id: {}", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Override
    public void updateDeliveryPartner(String id, DeliveryPartnerForm form) throws DeliveryPartnerException {
        log.info("Updating DeliveryPartnerForm by id: {}", id);

        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_DELIVERY_PARTNER_ENTITY_ID.getValue(), id);
        Optional<DeliveryPartnerDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_NO_DELIVERY_PARTNER_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_FOUND_DELIVERY_PARTNER_ENTITY_ID.getValue(), id);

        DeliveryPartnerDocument actualDocument = optActualDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("DeliveryPartnerDocument is inactive with id: {}", id);
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("DeliveryPartnerDocument is active with id: {}", id);

        if(form == null) {
            log.debug("DeliveryPartnerForm is null");
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of DeliveryPartnerForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("DeliveryPartnerForm has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("DeliveryPartnerForm error detail: {}", ec);
            throw new DeliveryPartnerException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of DeliveryPartnerForm are empty");
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of DeliveryPartnerForm are valid");

        Optional<DeliveryPartnerDocument> optExpectedDocument = form2DocumentMapper.compareAndMap(actualDocument, form);
        if(optExpectedDocument.isEmpty()) {
            log.debug("No new value for attributes of DeliveryPartnerForm");
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from DeliveryPartnerForm to DeliveryPartnerDocument");

        DeliveryPartnerDocument expectedDocument = optExpectedDocument.get();

        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_DELIVERY_PARTNER_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(actualDocument.getName().compareTo(expectedDocument.getName()) == 0
                || repository.existsByName(expectedDocument.getName())) {
            log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_DELIVERY_PARTNER_EXISTS_BY_NAME.getValue(), expectedDocument.getName());
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_EXISTS,
                    new Object[]{ "name", actualDocument.getName() });
        }
        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_DELIVERY_PARTNER_NON_EXISTENCE_BY_NAME.getValue(), expectedDocument.getName());

        entitySelfMapper.compareAndMap(expectedDocument, actualDocument);
        log.debug("Compared and copied attributes from DeliveryPartnerDocument to DeliveryPartnerForm");
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Updated: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to update {}", actualDocument);
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency deliveryPartner details" });
        }
        log.info("Updated existing DeliveryPartnerDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void deleteDeliveryPartner(String id) throws DeliveryPartnerException {
        log.info("Soft deleting DeliveryPartnerDocument by id: {}", id);

        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_DELIVERY_PARTNER_ENTITY_ID.getValue(), id);
        Optional<DeliveryPartnerDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_NO_DELIVERY_PARTNER_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_FOUND_DELIVERY_PARTNER_ENTITY_ID.getValue(), id);

        DeliveryPartnerDocument actualDocument = optDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("DeliveryPartnerDocument is inactive with id: {}", id);
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("DeliveryPartnerDocument is active with id: {}", id);

        actualDocument.setActive(Boolean.FALSE);
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualDocument);
        DeliveryPartnerDocument expectedDocument = repository.save(actualDocument);
        log.debug("Soft deleted: {}", expectedDocument);
        if(expectedDocument == null) {
            log.debug("Unable to soft delete {}", actualDocument);
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current deliveryPartner details with id:" + id });
        }

        log.info("Soft deleted existing DeliveryPartnerDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void applyPatchOnDeliveryPartner(String id, List<PatchOperationForm> patches) throws DeliveryPartnerException {
        log.info("Patching DeliveryPartnerDocument by id: {}", id);

        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_DELIVERY_PARTNER_ENTITY_ID.getValue(), id);
        Optional<DeliveryPartnerDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_NO_DELIVERY_PARTNER_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_FOUND_DELIVERY_PARTNER_ENTITY_ID.getValue(), id);

        DeliveryPartnerDocument actualDocument = optActualDocument.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("DeliveryPartner patch list not provided");
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("DeliveryPartner patch list has {} items", patches.size());


        log.debug("Validating patch list items for DeliveryPartner");
        try {
            toabBaseService.validatePatches(patches, SettingsErrorCode.SETTINGS_EXISTS.getDomain() + ":LOV");
            log.debug("All DeliveryPartner patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the DeliveryPartner patch item are invalid");
            throw new DeliveryPartnerException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for DeliveryPartner");


        log.debug("Patching list items to DeliveryPartnerDto");
        DeliveryPartnerDto patchedDeliveryPartnerForm = new DeliveryPartnerDto();
        try {
            log.debug("Preparing patch list items for DeliveryPartner");
            JsonNode deliveryPartnerDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch deliveryPartnerPatch = JsonPatch.fromJson(deliveryPartnerDtoTree);
            log.debug("Prepared patch list items for DeliveryPartner");
            JsonNode blankDeliveryPartnerDtoTree = om.convertValue(new DeliveryPartnerDto(), JsonNode.class);
            JsonNode patchedDeliveryPartnerFormTree = deliveryPartnerPatch.apply(blankDeliveryPartnerDtoTree);
            log.debug("Applying patch list items to DeliveryPartnerDto");
            patchedDeliveryPartnerForm = om.treeToValue(patchedDeliveryPartnerFormTree, DeliveryPartnerDto.class);
            log.debug("Applied patch list items to DeliveryPartnerDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to DeliveryPartnerDto: {}", e);
            DeliveryPartnerException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in DeliveryPartnerDto");
                ex = new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to DeliveryPartnerDto: {}", e);
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to DeliveryPartnerDto");

        log.debug("Validating patched DeliveryPartnerDto");
        Errors err = new DirectFieldBindingResult(patchedDeliveryPartnerForm, patchedDeliveryPartnerForm.getClass().getSimpleName());
        dtoValidator.validate(patchedDeliveryPartnerForm, err);
        if(err.hasErrors()) {
            log.debug("Patched DeliveryPartnerDto has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched DeliveryPartnerDto error detail: {}", ec);
            throw new DeliveryPartnerException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched DeliveryPartnerDto are valid");

        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_DELIVERY_PARTNER_EXISTENCE_BY_NAME.getValue(), patchedDeliveryPartnerForm.getName().get());
        if(actualDocument.getName().compareTo(patchedDeliveryPartnerForm.getName().get()) == 0
                || repository.existsByName(patchedDeliveryPartnerForm.getName().get())) {
            log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_DELIVERY_PARTNER_EXISTS_BY_NAME.getValue(), patchedDeliveryPartnerForm.getName().get());
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_EXISTS,
                    new Object[]{ "name", patchedDeliveryPartnerForm.getName().get() });
        }
        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_DELIVERY_PARTNER_NON_EXISTENCE_BY_NAME.getValue(), patchedDeliveryPartnerForm.getName().get());


        log.debug("Comparatively copying patched attributes from DeliveryPartnerDto to DeliveryPartnerDocument");
        try {
            dto2DocumentConverter.compareAndMap(patchedDeliveryPartnerForm, actualDocument);
        } catch (TOABBaseException e) {
            throw (DeliveryPartnerException) e;
        }
        log.debug("Comparatively copied patched attributes from DeliveryPartnerDto to DeliveryPartnerDocument");

        log.debug("Saving patched DeliveryPartnerDocument: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Saved patched DeliveryPartnerDocument: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to patch delete DeliveryPartnerDocument with id:{}", id);
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency deliveryPartner details with id:" + id });
        }
        log.info("Patched DeliveryPartnerDocument with id:{}", id);
    }
}