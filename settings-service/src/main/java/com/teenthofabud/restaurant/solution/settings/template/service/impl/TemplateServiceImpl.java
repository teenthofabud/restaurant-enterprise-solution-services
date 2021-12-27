package com.teenthofabud.restaurant.solution.settings.template.service.impl;

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
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import com.teenthofabud.restaurant.solution.settings.template.converter.TemplateDto2DocumentConverter;
import com.teenthofabud.restaurant.solution.settings.template.converter.TemplateForm2DocumentConverter;
import com.teenthofabud.restaurant.solution.settings.template.data.*;
import com.teenthofabud.restaurant.solution.settings.template.mapper.TemplateDocumentSelfMapper;
import com.teenthofabud.restaurant.solution.settings.template.mapper.TemplateForm2DocumentMapper;
import com.teenthofabud.restaurant.solution.settings.template.repository.TemplateRepository;
import com.teenthofabud.restaurant.solution.settings.template.service.TemplateService;
import com.teenthofabud.restaurant.solution.settings.template.validator.TemplateDtoValidator;
import com.teenthofabud.restaurant.solution.settings.template.validator.TemplateFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.settings.template.validator.TemplateFormValidator;
import com.teenthofabud.restaurant.solution.settings.utils.SettingsServiceHelper;
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

@Component
@Slf4j
public class TemplateServiceImpl implements TemplateService {

    private static final Comparator<TemplateVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private TemplateForm2DocumentConverter form2DocumentConverter;
    private TemplateDto2DocumentConverter dto2DocumentConverter;
    private TemplateForm2DocumentMapper form2DocumentMapper;
    private TemplateDocumentSelfMapper documentSelfMapper;
    private TemplateFormValidator formValidator;
    private TemplateFormRelaxedValidator relaxedFormValidator;
    private TemplateDtoValidator dtoValidator;
    private TemplateRepository repository;
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
    public void setDto2DocumentConverter(TemplateDto2DocumentConverter dto2DocumentConverter) {
        this.dto2DocumentConverter = dto2DocumentConverter;
    }

    @Autowired
    public void setForm2DocumentMapper(TemplateForm2DocumentMapper form2DocumentMapper) {
        this.form2DocumentMapper = form2DocumentMapper;
    }

    @Autowired
    public void setDocumentSelfMapper(TemplateDocumentSelfMapper documentSelfMapper) {
        this.documentSelfMapper = documentSelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(TemplateFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchTemplateValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(TemplateDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2DocumentConverter(TemplateForm2DocumentConverter form2DocumentConverter) {
        this.form2DocumentConverter = form2DocumentConverter;
    }

    @Autowired
    public void setRepository(TemplateRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(TemplateFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseTemplateId(String id) throws TemplateException {
        Long templateId = null;
        try {
            templateId = Long.parseLong(id);
            log.debug("Parsed id {} to template id {} in numeric format", id, templateId);
            if(templateId <= 0) {
                throw new NumberFormatException("template id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse template id", e);
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_ID_INVALID.getValue(), id);
            throw new TemplateException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return templateId;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public Set<TemplateVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all TemplateDocument by their natural ordering");
        List<TemplateDocument> templateDocumentList = repository.findAll();
        List<TemplateVo> templateVoList = settingsServiceHelper.templateDocument2DetailedVo(templateDocumentList);
        Set<TemplateVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_NAME);
        naturallyOrderedSet.addAll(templateVoList);
        log.info("{} TemplateVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public TemplateVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws TemplateException {
        log.info("Requesting TemplateDocument by id: {}", id);
        Optional<TemplateDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug("No TemplateDocument found by id: {}", id);
            throw new TemplateException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found TemplateVo by id: {}", id);
        TemplateDocument document = optDocument.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        TemplateVo vo = settingsServiceHelper.templateDocument2DetailedVo(document);
        log.debug("TemplateVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<TemplateVo> retrieveAllMatchingDetailsByCriteria(
            Optional<String> optionalName, Optional<String> optionalDescription, Optional<String> optionalTemplateTypeId) throws TemplateException {
        if(optionalName.isEmpty() && optionalDescription.isEmpty() && optionalTemplateTypeId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String description = optionalDescription.isPresent() ? optionalDescription.get() : "";
        String templateTypeId = optionalTemplateTypeId.isPresent() ? optionalTemplateTypeId.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(description))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(templateTypeId))) {
            log.debug("All search parameters are empty");
        }
        List<TemplateVo> matchedTemplateList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        TemplateDocument document = new TemplateDocument();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            log.debug("name {} is valid", name);
            providedFilters.put("name", name);
            document.setName(name);
            matcherCriteria = matcherCriteria.withMatcher("name", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(description))) {
            log.debug("description {} is valid", description);
            providedFilters.put("description", description);
            document.setDescription(description);
            matcherCriteria = matcherCriteria.withMatcher("description", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(templateTypeId))) {
            try {
                TemplateType.valueOf(templateTypeId);
            } catch (IllegalArgumentException e) {
                log.error("templateTypeId parameter is invalid", e);
                throw new TemplateException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "templateTypeId", templateTypeId });
            }
            log.debug("templateTypeId {} is valid", description);
            providedFilters.put("templateTypeId", description);
            document.setTemplateTypeId(templateTypeId);
            matcherCriteria = matcherCriteria.withMatcher("templateTypeId", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<TemplateDocument> templateDocumentExample = Example.of(document, matcherCriteria);
        List<TemplateDocument> templateDocumentList = repository.findAll(templateDocumentExample);
        matchedTemplateList = settingsServiceHelper.templateDocument2DetailedVo(templateDocumentList);
        log.info("Found {} TemplateVo matching with provided parameters : {}", matchedTemplateList.size(), providedFilters);
        log.info("No TemplateVo available matching with provided parameters : {}", matchedTemplateList.size(), providedFilters);
        return matchedTemplateList;
    }

    @Transactional
    @Override
    public String createTemplate(TemplateForm form) throws TemplateException {
        log.info("Creating new TemplateDocument");

        if(form == null) {
            log.debug("TemplateForm provided is null");
            throw new TemplateException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of TemplateForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("TemplateForm has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TemplateForm error detail: {}", ec);
            throw new TemplateException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of TemplateForm are valid");

        TemplateDocument expectedDocument = form2DocumentConverter.convert(form);

        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(), form.getName(), form.getTemplateTypeId());
        if(repository.existsByNameAndTemplateTypeId(expectedDocument.getName(), expectedDocument.getTemplateTypeId())) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(), expectedDocument.getName());
            throw new TemplateException(SettingsErrorCode.SETTINGS_EXISTS,
                    new Object[]{ "name: " + form.getName(), ", templateTypeId: " + form.getTemplateTypeId() });
        }
        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(), expectedDocument.getName());

        log.debug("Saving {}", expectedDocument);
        TemplateDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null) {
            log.debug("Unable to create {}", expectedDocument);
            throw new TemplateException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist TemplateForm details" });
        }
        log.info("Created new TemplateForm with id: {}", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Transactional
    @Override
    public void updateTemplate(String id, TemplateForm form) throws TemplateException {
        log.info("Updating TemplateForm by id: {}", id);

        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TEMPLATE_ENTITY_ID.getValue(), id);
        Optional<TemplateDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_NO_TEMPLATE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TemplateException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_FOUND_TEMPLATE_ENTITY_ID.getValue(), id);

        TemplateDocument actualDocument = optActualDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("TemplateDocument is inactive with id: {}", id);
            throw new TemplateException(SettingsErrorCode.SETTINGS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TemplateDocument is active with id: {}", id);

        if(form == null) {
            log.debug("TemplateForm is null");
            throw new TemplateException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of TemplateForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("TemplateForm has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TemplateForm error detail: {}", ec);
            throw new TemplateException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of TemplateForm are empty");
            throw new TemplateException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of TemplateForm are valid");

        Optional<TemplateDocument> optExpectedDocument = form2DocumentMapper.compareAndMap(actualDocument, form);
        if(optExpectedDocument.isEmpty()) {
            log.debug("No new value for attributes of TemplateForm");
            throw new TemplateException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from TemplateForm to TemplateDocument");

        TemplateDocument expectedDocument = optExpectedDocument.get();

        checkUniquenessOfTemplate(form, actualDocument);

        documentSelfMapper.compareAndMap(expectedDocument, actualDocument);
        log.debug("Compared and copied attributes from TemplateDocument to TemplateForm");
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Updated: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to update {}", actualDocument);
            throw new TemplateException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency template details" });
        }
        log.info("Updated existing TemplateDocument with id: {}", actualDocument.getId());
    }

    @Transactional
    @Override
    public void deleteTemplate(String id) throws TemplateException {
        log.info("Soft deleting TemplateDocument by id: {}", id);

        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TEMPLATE_ENTITY_ID.getValue(), id);
        Optional<TemplateDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_NO_TEMPLATE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TemplateException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_FOUND_TEMPLATE_ENTITY_ID.getValue(), id);

        TemplateDocument actualDocument = optDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("TemplateDocument is inactive with id: {}", id);
            throw new TemplateException(SettingsErrorCode.SETTINGS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TemplateDocument is active with id: {}", id);

        actualDocument.setActive(Boolean.FALSE);
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualDocument);
        TemplateDocument expectedDocument = repository.save(actualDocument);
        log.debug("Soft deleted: {}", expectedDocument);
        if(expectedDocument == null) {
            log.debug("Unable to soft delete {}", actualDocument);
            throw new TemplateException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current template details with id:" + id });
        }

        log.info("Soft deleted existing TemplateDocument with id: {}", actualDocument.getId());
    }

    @Transactional
    @Override
    public void applyPatchOnTemplate(String id, List<PatchOperationForm> patches) throws TemplateException {
        log.info("Patching TemplateDocument by id: {}", id);

        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TEMPLATE_ENTITY_ID.getValue(), id);
        Optional<TemplateDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_NO_TEMPLATE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TemplateException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_FOUND_TEMPLATE_ENTITY_ID.getValue(), id);

        TemplateDocument actualDocument = optActualDocument.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Template patch list not provided");
            throw new TemplateException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Template patch list has {} items", patches.size());


        log.debug("Validating patch list items for Template");
        try {
            toabBaseService.validatePatches(patches, SettingsErrorCode.SETTINGS_EXISTS.getDomain() + ":LOV");
            log.debug("All Template patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Template patch item are invalid");
            throw new TemplateException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Template");


        log.debug("Patching list items to TemplateDto");
        TemplateDto patchedTemplateForm = new TemplateDto();
        try {
            log.debug("Preparing patch list items for Template");
            JsonNode templateDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch templatePatch = JsonPatch.fromJson(templateDtoTree);
            log.debug("Prepared patch list items for Template");
            JsonNode blankTemplateDtoTree = om.convertValue(new TemplateDto(), JsonNode.class);
            JsonNode patchedTemplateFormTree = templatePatch.apply(blankTemplateDtoTree);
            log.debug("Applying patch list items to TemplateDto");
            patchedTemplateForm = om.treeToValue(patchedTemplateFormTree, TemplateDto.class);
            log.debug("Applied patch list items to TemplateDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to TemplateDto: {}", e);
            TemplateException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in TemplateDto");
                ex = new TemplateException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new TemplateException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to TemplateDto: {}", e);
            throw new TemplateException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to TemplateDto");

        log.debug("Validating patched TemplateDto");
        Errors err = new DirectFieldBindingResult(patchedTemplateForm, patchedTemplateForm.getClass().getSimpleName());
        dtoValidator.validate(patchedTemplateForm, err);
        if(err.hasErrors()) {
            log.debug("Patched TemplateDto has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched TemplateDto error detail: {}", ec);
            throw new TemplateException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched TemplateDto are valid");

        checkUniquenessOfTemplate(patchedTemplateForm, actualDocument);

        log.debug("Comparatively copying patched attributes from TemplateDto to TemplateDocument");
        try {
            dto2DocumentConverter.compareAndMap(patchedTemplateForm, actualDocument);
        } catch (TOABBaseException e) {
            throw (TemplateException) e;
        }
        log.debug("Comparatively copied patched attributes from TemplateDto to TemplateDocument");

        log.debug("Saving patched TemplateDocument: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Saved patched TemplateDocument: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to patch delete TemplateDocument with id:{}", id);
            throw new TemplateException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency template details with id:" + id });
        }
        log.info("Patched TemplateDocument with id:{}", id);
    }

    private void checkUniquenessOfTemplate(TemplateDto patchedTemplateForm, TemplateDocument actualDocument) throws TemplateException {
        // name = true, templateTypeId = false
        if(patchedTemplateForm.getName().isPresent() && patchedTemplateForm.getTemplateTypeId().isEmpty()) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    patchedTemplateForm.getName().get(), actualDocument.getTemplateTypeId());
            boolean sameDocumentSw = patchedTemplateForm.getName().get().compareTo(actualDocument.getName()) == 0;
            boolean duplicateDocumentSw =  repository.existsByNameAndTemplateTypeId(patchedTemplateForm.getName().get(), actualDocument.getTemplateTypeId());
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        patchedTemplateForm.getName().get(), actualDocument.getTemplateTypeId());
                throw new TemplateException(SettingsErrorCode.SETTINGS_EXISTS,
                        new Object[]{ "name: " + patchedTemplateForm.getName().get(), ", templateTypeId: " + actualDocument.getTemplateTypeId() });
            }
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    patchedTemplateForm.getName().get(), actualDocument.getTemplateTypeId());
        }

        // name = true, templateTypeId = true
        if(patchedTemplateForm.getName().isPresent() && patchedTemplateForm.getTemplateTypeId().isPresent()) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    patchedTemplateForm.getName().get(), patchedTemplateForm.getTemplateTypeId().get());
            boolean sameDocumentSw = patchedTemplateForm.getName().get().compareTo(actualDocument.getName()) == 0
                    && patchedTemplateForm.getTemplateTypeId().get().compareTo(actualDocument.getTemplateTypeId()) == 0;
            boolean duplicateDocumentSw =  repository.existsByNameAndTemplateTypeId(patchedTemplateForm.getName().get(),
                    patchedTemplateForm.getTemplateTypeId().get());
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        patchedTemplateForm.getName().get(), patchedTemplateForm.getTemplateTypeId().get());
                throw new TemplateException(SettingsErrorCode.SETTINGS_EXISTS,
                        new Object[]{ "name: " + patchedTemplateForm.getName().get(), ", templateTypeId: " + patchedTemplateForm.getTemplateTypeId().get() });
            }
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    patchedTemplateForm.getName().get(), patchedTemplateForm.getTemplateTypeId().get());
        }

        // name = false, templateTypeId = true
        if(patchedTemplateForm.getName().isEmpty() && patchedTemplateForm.getTemplateTypeId().isPresent()) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    actualDocument.getName(),  patchedTemplateForm.getTemplateTypeId().get());
            boolean sameDocumentSw = patchedTemplateForm.getTemplateTypeId().get().compareTo(actualDocument.getTemplateTypeId()) == 0;
            boolean duplicateDocumentSw =  repository.existsByNameAndTemplateTypeId(
                    actualDocument.getName(), patchedTemplateForm.getTemplateTypeId().get());
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        actualDocument.getName(), patchedTemplateForm.getTemplateTypeId().get());
                throw new TemplateException(SettingsErrorCode.SETTINGS_EXISTS, new Object[]{ "name " + actualDocument.getName(),
                        ", templateTypeId: " + patchedTemplateForm.getTemplateTypeId().get() });
            }
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    actualDocument.getName(),  patchedTemplateForm.getTemplateTypeId().get());
        }
    }

    private void checkUniquenessOfTemplate(TemplateForm templateForm, TemplateDocument actualDocument) throws TemplateException {
        // name = true, templateTypeId = false
        if(StringUtils.hasText(StringUtils.trimWhitespace(templateForm.getName()))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(templateForm.getTemplateTypeId()))) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    templateForm.getName(), actualDocument.getTemplateTypeId());
            boolean sameDocumentSw = templateForm.getName().compareTo(actualDocument.getName()) == 0;
            boolean duplicateDocumentSw =  repository.existsByNameAndTemplateTypeId(templateForm.getName(), actualDocument.getTemplateTypeId());
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        templateForm.getName(), actualDocument.getTemplateTypeId());
                throw new TemplateException(SettingsErrorCode.SETTINGS_EXISTS,
                        new Object[]{ "name: " + templateForm.getName(), ", templateTypeId: " + actualDocument.getTemplateTypeId() });
            }
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    templateForm.getName(), actualDocument.getTemplateTypeId());
        }

        // name = true, templateTypeId = true
        if(StringUtils.hasText(StringUtils.trimWhitespace(templateForm.getName()))
                && StringUtils.hasText(StringUtils.trimWhitespace(templateForm.getTemplateTypeId()))) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    templateForm.getName(), templateForm.getTemplateTypeId());
            boolean sameDocumentSw = templateForm.getName().compareTo(actualDocument.getName()) == 0
                    && templateForm.getTemplateTypeId().compareTo(actualDocument.getTemplateTypeId()) == 0;
            boolean duplicateDocumentSw =  repository.existsByNameAndTemplateTypeId(templateForm.getName(), templateForm.getTemplateTypeId());
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        templateForm.getName(), templateForm.getTemplateTypeId());
                throw new TemplateException(SettingsErrorCode.SETTINGS_EXISTS,
                        new Object[]{ "name: " + templateForm.getName(), ", templateTypeId: " + templateForm.getTemplateTypeId() });
            }
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    templateForm.getName(), templateForm.getTemplateTypeId());
        }

        // name = false, templateTypeId = true
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(templateForm.getName()))
                && StringUtils.hasText(StringUtils.trimWhitespace(templateForm.getTemplateTypeId()))) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    actualDocument.getName(),  templateForm.getTemplateTypeId());
            boolean sameDocumentSw = templateForm.getName().compareTo(actualDocument.getName()) == 0
                    && templateForm.getTemplateTypeId().compareTo(actualDocument.getTemplateTypeId()) == 0;
            boolean duplicateDocumentSw =  repository.existsByNameAndTemplateTypeId(actualDocument.getName(),
                    templateForm.getTemplateTypeId());
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        actualDocument.getName(),  templateForm.getTemplateTypeId());
                throw new TemplateException(SettingsErrorCode.SETTINGS_EXISTS, new Object[]{ "name: " + actualDocument.getName(),
                        ", templateTypeId: " + templateForm.getTemplateTypeId() });
            }
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    actualDocument.getName(),  templateForm.getTemplateTypeId());
        }
    }
    
}