package com.teenthofabud.restaurant.solution.print.template.service.impl;

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
import com.teenthofabud.restaurant.solution.print.template.converter.TemplateDto2EntityConverter;
import com.teenthofabud.restaurant.solution.print.template.converter.TemplateForm2EntityConverter;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateDto;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateEntity;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateException;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateForm;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateMessageTemplate;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateType;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateVo;
import com.teenthofabud.restaurant.solution.print.template.mapper.TemplateEntitySelfMapper;
import com.teenthofabud.restaurant.solution.print.template.mapper.TemplateForm2EntityMapper;
import com.teenthofabud.restaurant.solution.print.template.repository.TemplateRepository;
import com.teenthofabud.restaurant.solution.print.template.service.TemplateService;
import com.teenthofabud.restaurant.solution.print.template.validator.TemplateDtoValidator;
import com.teenthofabud.restaurant.solution.print.template.validator.TemplateFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.print.template.validator.TemplateFormValidator;
import com.teenthofabud.restaurant.solution.print.error.PrintErrorCode;
import com.teenthofabud.restaurant.solution.print.utils.PrintServiceHelper;
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

    private TemplateForm2EntityConverter form2EntityConverter;
    private TemplateDto2EntityConverter dto2EntityConverter;
    private TemplateForm2EntityMapper form2EntityMapper;
    private TemplateEntitySelfMapper entitySelfMapper;
    private TemplateFormValidator formValidator;
    private TemplateFormRelaxedValidator relaxedFormValidator;
    private TemplateDtoValidator dtoValidator;
    private TemplateRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private PrintServiceHelper printServiceHelper;

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }


    @Autowired
    public void setPrintServiceHelper(PrintServiceHelper printServiceHelper) {
        this.printServiceHelper = printServiceHelper;
    }

    @Autowired
    public void setDto2EntityConverter(TemplateDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(TemplateForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(TemplateEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
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
    public void setForm2EntityConverter(TemplateForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
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
            throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return templateId;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public Set<TemplateVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all TemplateEntity by their natural ordering");
        List<TemplateEntity> templateEntityList = repository.findAll();
        List<TemplateVo> templateVoList = printServiceHelper.templateEntity2DetailedVo(templateEntityList);
        Set<TemplateVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_NAME);
        naturallyOrderedSet.addAll(templateVoList);
        log.info("{} TemplateVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public TemplateVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws TemplateException {
        log.info("Requesting TemplateEntity by id: {}", id);
        Long templateId = parseTemplateId(id);
        Optional<TemplateEntity> optEntity = repository.findById(templateId);
        if(optEntity.isEmpty()) {
            log.debug("No TemplateEntity found by id: {}", id);
            throw new TemplateException(PrintErrorCode.PRINT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found TemplateVo by id: {}", id);
        TemplateEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        TemplateVo vo = printServiceHelper.templateEntity2DetailedVo(entity);
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
        TemplateEntity entity = new TemplateEntity();
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
        if(StringUtils.hasText(StringUtils.trimWhitespace(templateTypeId))) {
            try {
                TemplateType.valueOf(templateTypeId);
            } catch (IllegalArgumentException e) {
                log.error("templateTypeId parameter is invalid", e);
                throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_INVALID, new Object[] { "templateTypeId", templateTypeId });
            }
            log.debug("templateTypeId {} is valid", description);
            providedFilters.put("templateTypeId", description);
            entity.setTemplateTypeId(templateTypeId);
            matcherCriteria = matcherCriteria.withMatcher("templateTypeId", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<TemplateEntity> templateEntityExample = Example.of(entity, matcherCriteria);
        List<TemplateEntity> templateEntityList = repository.findAll(templateEntityExample);
        matchedTemplateList = printServiceHelper.templateEntity2DetailedVo(templateEntityList);
        log.info("Found {} TemplateVo matching with provided parameters : {}", matchedTemplateList.size(), providedFilters);
        log.info("No TemplateVo available matching with provided parameters : {}", matchedTemplateList.size(), providedFilters);
        return matchedTemplateList;
    }

    @Transactional
    @Override
    public String createTemplate(TemplateForm form) throws TemplateException {
        log.info("Creating new TemplateEntity");

        if(form == null) {
            log.debug("TemplateForm provided is null");
            throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of TemplateForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("TemplateForm has {} errors", err.getErrorCount());
            PrintErrorCode ec = PrintErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TemplateForm error detail: {}", ec);
            throw new TemplateException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of TemplateForm are valid");

        TemplateEntity expectedEntity = form2EntityConverter.convert(form);

        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(), form.getName());
        if(repository.existsByNameAndTemplateTypeId(expectedEntity.getName(), expectedEntity.getTemplateTypeId())) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(), expectedEntity.getName());
            throw new TemplateException(PrintErrorCode.PRINT_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(), expectedEntity.getName());

        log.debug("Saving {}", expectedEntity);
        TemplateEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new TemplateException(PrintErrorCode.PRINT_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist TemplateForm details" });
        }
        log.info("Created new TemplateForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Transactional
    @Override
    public void updateTemplate(String id, TemplateForm form) throws TemplateException {
        log.info("Updating TemplateForm by id: {}", id);

        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TEMPLATE_ENTITY_ID.getValue(), id);
        Long templateId = parseTemplateId(id);
        Optional<TemplateEntity> optActualEntity = repository.findById(templateId);
        if(optActualEntity.isEmpty()) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_NO_TEMPLATE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TemplateException(PrintErrorCode.PRINT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_FOUND_TEMPLATE_ENTITY_ID.getValue(), id);

        TemplateEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TemplateEntity is inactive with id: {}", id);
            throw new TemplateException(PrintErrorCode.PRINT_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TemplateEntity is active with id: {}", id);

        if(form == null) {
            log.debug("TemplateForm is null");
            throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of TemplateForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("TemplateForm has {} errors", err.getErrorCount());
            PrintErrorCode ec = PrintErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TemplateForm error detail: {}", ec);
            throw new TemplateException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of TemplateForm are empty");
            throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of TemplateForm are valid");

        Optional<TemplateEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of TemplateForm");
            throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from TemplateForm to TemplateEntity");

        TemplateEntity expectedEntity = optExpectedEntity.get();

        checkUniquenessOfTemplate(form, actualEntity);

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from TemplateEntity to TemplateForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new TemplateException(PrintErrorCode.PRINT_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency template details" });
        }
        log.info("Updated existing TemplateEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void deleteTemplate(String id) throws TemplateException {
        log.info("Soft deleting TemplateEntity by id: {}", id);

        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TEMPLATE_ENTITY_ID.getValue(), id);
        Long templateId = parseTemplateId(id);
        Optional<TemplateEntity> optEntity = repository.findById(templateId);
        if(optEntity.isEmpty()) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_NO_TEMPLATE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TemplateException(PrintErrorCode.PRINT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_FOUND_TEMPLATE_ENTITY_ID.getValue(), id);

        TemplateEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TemplateEntity is inactive with id: {}", id);
            throw new TemplateException(PrintErrorCode.PRINT_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TemplateEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        TemplateEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new TemplateException(PrintErrorCode.PRINT_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current template details with id:" + id });
        }

        log.info("Soft deleted existing TemplateEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void applyPatchOnTemplate(String id, List<PatchOperationForm> patches) throws TemplateException {
        log.info("Patching TemplateEntity by id: {}", id);

        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TEMPLATE_ENTITY_ID.getValue(), id);
        Long templateId = parseTemplateId(id);
        Optional<TemplateEntity> optActualEntity = repository.findById(templateId);
        if(optActualEntity.isEmpty()) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_NO_TEMPLATE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TemplateException(PrintErrorCode.PRINT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_FOUND_TEMPLATE_ENTITY_ID.getValue(), id);

        TemplateEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Template patch list not provided");
            throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Template patch list has {} items", patches.size());


        log.debug("Validating patch list items for Template");
        try {
            toabBaseService.validatePatches(patches, PrintErrorCode.PRINT_EXISTS.getDomain() + ":LOV");
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
                ex = new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new TemplateException(PrintErrorCode.PRINT_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to TemplateDto: {}", e);
            throw new TemplateException(PrintErrorCode.PRINT_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to TemplateDto");

        log.debug("Validating patched TemplateDto");
        Errors err = new DirectFieldBindingResult(patchedTemplateForm, patchedTemplateForm.getClass().getSimpleName());
        dtoValidator.validate(patchedTemplateForm, err);
        if(err.hasErrors()) {
            log.debug("Patched TemplateDto has {} errors", err.getErrorCount());
            PrintErrorCode ec = PrintErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched TemplateDto error detail: {}", ec);
            throw new TemplateException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched TemplateDto are valid");

        checkUniquenessOfTemplate(patchedTemplateForm, actualEntity);

        log.debug("Comparatively copying patched attributes from TemplateDto to TemplateEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedTemplateForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (TemplateException) e;
        }
        log.debug("Comparatively copied patched attributes from TemplateDto to TemplateEntity");

        log.debug("Saving patched TemplateEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched TemplateEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete TemplateEntity with id:{}", id);
            throw new TemplateException(PrintErrorCode.PRINT_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency template details with id:" + id });
        }
        log.info("Patched TemplateEntity with id:{}", id);
    }

    private void checkUniquenessOfTemplate(TemplateDto patchedTemplateForm, TemplateEntity actualEntity) throws TemplateException {
        // name = true, templateTypeId = false
        if(patchedTemplateForm.getName().isPresent() && patchedTemplateForm.getTemplateTypeId().isEmpty()) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    patchedTemplateForm.getName().get(), actualEntity.getTemplateTypeId());
            boolean sameEntitySw = patchedTemplateForm.getName().get().compareTo(actualEntity.getName()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndTemplateTypeId(patchedTemplateForm.getName().get(), actualEntity.getTemplateTypeId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        patchedTemplateForm.getName().get(), actualEntity.getTemplateTypeId());
                throw new TemplateException(PrintErrorCode.PRINT_EXISTS,
                        new Object[]{ "name: " + patchedTemplateForm.getName().get(), ", templateTypeId: " + actualEntity.getTemplateTypeId() });
            }
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    patchedTemplateForm.getName().get(), actualEntity.getTemplateTypeId());
        }

        // name = true, templateTypeId = true
        if(patchedTemplateForm.getName().isPresent() && patchedTemplateForm.getTemplateTypeId().isPresent()) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    patchedTemplateForm.getName().get(), patchedTemplateForm.getTemplateTypeId().get());
            boolean sameEntitySw = patchedTemplateForm.getName().get().compareTo(actualEntity.getName()) == 0
                    && patchedTemplateForm.getTemplateTypeId().get().compareTo(actualEntity.getTemplateTypeId()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndTemplateTypeId(patchedTemplateForm.getName().get(),
                    patchedTemplateForm.getTemplateTypeId().get());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        patchedTemplateForm.getName().get(), patchedTemplateForm.getTemplateTypeId().get());
                throw new TemplateException(PrintErrorCode.PRINT_EXISTS,
                        new Object[]{ "name: " + patchedTemplateForm.getName().get(), ", templateTypeId: " + patchedTemplateForm.getTemplateTypeId().get() });
            }
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    patchedTemplateForm.getName().get(), patchedTemplateForm.getTemplateTypeId().get());
        }

        // name = false, templateTypeId = true
        if(patchedTemplateForm.getName().isEmpty() && patchedTemplateForm.getTemplateTypeId().isPresent()) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    actualEntity.getName(),  patchedTemplateForm.getTemplateTypeId().get());
            boolean sameEntitySw = patchedTemplateForm.getTemplateTypeId().get().compareTo(actualEntity.getTemplateTypeId()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndTemplateTypeId(
                    actualEntity.getName(), patchedTemplateForm.getTemplateTypeId().get());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        actualEntity.getName(), patchedTemplateForm.getTemplateTypeId().get());
                throw new TemplateException(PrintErrorCode.PRINT_EXISTS, new Object[]{ "name " + actualEntity.getName(),
                        ", templateTypeId: " + patchedTemplateForm.getTemplateTypeId().get() });
            }
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    actualEntity.getName(),  patchedTemplateForm.getTemplateTypeId().get());
        }
    }

    private void checkUniquenessOfTemplate(TemplateForm templateForm, TemplateEntity actualEntity) throws TemplateException {
        // name = true, templateTypeId = false
        if(StringUtils.hasText(StringUtils.trimWhitespace(templateForm.getName()))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(templateForm.getTemplateTypeId()))) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    templateForm.getName(), actualEntity.getTemplateTypeId());
            boolean sameEntitySw = templateForm.getName().compareTo(actualEntity.getName()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndTemplateTypeId(templateForm.getName(), actualEntity.getTemplateTypeId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        templateForm.getName(), actualEntity.getTemplateTypeId());
                throw new TemplateException(PrintErrorCode.PRINT_EXISTS,
                        new Object[]{ "name: " + templateForm.getName(), ", templateTypeId: " + actualEntity.getTemplateTypeId() });
            }
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    templateForm.getName(), actualEntity.getTemplateTypeId());
        }

        // name = true, templateTypeId = true
        if(StringUtils.hasText(StringUtils.trimWhitespace(templateForm.getName()))
                && StringUtils.hasText(StringUtils.trimWhitespace(templateForm.getTemplateTypeId()))) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    templateForm.getName(), templateForm.getTemplateTypeId());
            boolean sameEntitySw = templateForm.getName().compareTo(actualEntity.getName()) == 0
                    && templateForm.getTemplateTypeId().compareTo(actualEntity.getTemplateTypeId()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndTemplateTypeId(templateForm.getName(), templateForm.getTemplateTypeId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        templateForm.getName(), templateForm.getTemplateTypeId());
                throw new TemplateException(PrintErrorCode.PRINT_EXISTS,
                        new Object[]{ "name: " + templateForm.getName(), ", templateTypeId: " + templateForm.getTemplateTypeId() });
            }
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    templateForm.getName(), templateForm.getTemplateTypeId());
        }

        // name = false, templateTypeId = true
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(templateForm.getName()))
                && StringUtils.hasText(StringUtils.trimWhitespace(templateForm.getTemplateTypeId()))) {
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    actualEntity.getName(),  templateForm.getTemplateTypeId());
            boolean sameEntitySw = templateForm.getName().compareTo(actualEntity.getName()) == 0
                    && templateForm.getTemplateTypeId().compareTo(actualEntity.getTemplateTypeId()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndTemplateTypeId(actualEntity.getName(),
                    templateForm.getTemplateTypeId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        actualEntity.getName(),  templateForm.getTemplateTypeId());
                throw new TemplateException(PrintErrorCode.PRINT_EXISTS, new Object[]{ "name: " + actualEntity.getName(),
                        ", templateTypeId: " + templateForm.getTemplateTypeId() });
            }
            log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    actualEntity.getName(),  templateForm.getTemplateTypeId());
        }
    }
    
}