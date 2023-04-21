package com.teenthofabud.restaurant.solution.settings.paymentmethod.service.impl;

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
import com.teenthofabud.restaurant.solution.settings.paymentmethod.converter.PaymentMethodDto2DocumentConverter;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.converter.PaymentMethodForm2DocumentConverter;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.*;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.mapper.PaymentMethodDocumentSelfMapper;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.mapper.PaymentMethodForm2DocumentMapper;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.repository.PaymentMethodRepository;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.service.PaymentMethodService;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.validator.PaymentMethodDtoValidator;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.validator.PaymentMethodFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.validator.PaymentMethodFormValidator;
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
public class PaymentMethodServiceImpl implements PaymentMethodService {

    private static final Comparator<PaymentMethodVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private PaymentMethodForm2DocumentConverter form2DocumentConverter;
    private PaymentMethodDto2DocumentConverter dto2DocumentConverter;
    private PaymentMethodForm2DocumentMapper form2DocumentMapper;
    private PaymentMethodDocumentSelfMapper entitySelfMapper;
    private PaymentMethodFormValidator formValidator;
    private PaymentMethodFormRelaxedValidator relaxedFormValidator;
    private PaymentMethodDtoValidator dtoValidator;
    private PaymentMethodRepository repository;
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
    public void setDto2DocumentConverter(PaymentMethodDto2DocumentConverter dto2DocumentConverter) {
        this.dto2DocumentConverter = dto2DocumentConverter;
    }

    @Autowired
    public void setForm2DocumentMapper(PaymentMethodForm2DocumentMapper form2DocumentMapper) {
        this.form2DocumentMapper = form2DocumentMapper;
    }

    @Autowired
    public void setDocumentSelfMapper(PaymentMethodDocumentSelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(PaymentMethodFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchPaymentMethodValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(PaymentMethodDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2DocumentConverter(PaymentMethodForm2DocumentConverter form2DocumentConverter) {
        this.form2DocumentConverter = form2DocumentConverter;
    }

    @Autowired
    public void setRepository(PaymentMethodRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(PaymentMethodFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parsePaymentMethodId(String id) throws PaymentMethodException {
        Long paymentMethodId = null;
        try {
            paymentMethodId = Long.parseLong(id);
            log.debug("Parsed id {} to paymentMethod id {} in numeric format", id, paymentMethodId);
            if(paymentMethodId <= 0) {
                throw new NumberFormatException("paymentMethod id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse paymentMethod id", e);
            log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_ID_INVALID.getValue(), id);
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return paymentMethodId;
    }

    @Override
    public Set<PaymentMethodVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all PaymentMethodDocument by their natural ordering");
        List<PaymentMethodDocument> paymentMethodDocumentList = repository.findAll();
        List<PaymentMethodVo> paymentMethodVoList = settingsServiceHelper.paymentMethodDocument2DetailedVo(paymentMethodDocumentList);
        Set<PaymentMethodVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_NAME);
        naturallyOrderedSet.addAll(paymentMethodVoList);
        log.info("{} PaymentMethodVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public PaymentMethodVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws PaymentMethodException {
        log.info("Requesting PaymentMethodDocument by id: {}", id);
        Optional<PaymentMethodDocument> optDocument = repository.findById(id);
        if(!optDocument.isPresent()) {
            log.debug("No PaymentMethodDocument found by id: {}", id);
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found PaymentMethodVo by id: {}", id);
        PaymentMethodDocument entity = optDocument.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        PaymentMethodVo vo = settingsServiceHelper.paymentMethodDocument2DetailedVo(entity);
        log.debug("PaymentMethodVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<PaymentMethodVo> retrieveAllMatchingDetailsByCriteria(
            Optional<String> optionalName, Optional<String> optionalDescription) throws PaymentMethodException {
        if(!optionalName.isPresent() && !optionalDescription.isPresent()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String description = optionalDescription.isPresent() ? optionalDescription.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(description))) {
            log.debug("All search parameters are empty");
        }
        List<PaymentMethodVo> matchedPaymentMethodList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        PaymentMethodDocument entity = new PaymentMethodDocument();
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
        Example<PaymentMethodDocument> paymentMethodDocumentExample = Example.of(entity, matcherCriteria);
        List<PaymentMethodDocument> paymentMethodDocumentList = repository.findAll(paymentMethodDocumentExample);
        matchedPaymentMethodList = settingsServiceHelper.paymentMethodDocument2DetailedVo(paymentMethodDocumentList);
        log.info("Found {} PaymentMethodVo matching with provided parameters : {}", matchedPaymentMethodList.size(), providedFilters);
        log.info("No PaymentMethodVo available matching with provided parameters : {}", matchedPaymentMethodList.size(), providedFilters);
        return matchedPaymentMethodList;
    }

    @Override
    public String createPaymentMethod(PaymentMethodForm form) throws PaymentMethodException {
        log.info("Creating new PaymentMethodDocument");

        if(form == null) {
            log.debug("PaymentMethodForm provided is null");
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of PaymentMethodForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("PaymentMethodForm has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("PaymentMethodForm error detail: {}", ec);
            throw new PaymentMethodException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of PaymentMethodForm are valid");

        PaymentMethodDocument expectedDocument = form2DocumentConverter.convert(form);

        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(repository.existsByName(expectedDocument.getName())) {
            log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_EXISTS_BY_NAME.getValue(), expectedDocument.getName());
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_NON_EXISTENCE_BY_NAME.getValue(), expectedDocument.getName());

        log.debug("Saving {}", expectedDocument);
        PaymentMethodDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null) {
            log.debug("Unable to create {}", expectedDocument);
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist PaymentMethodForm details" });
        }
        log.info("Created new PaymentMethodForm with id: {}", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Override
    public void updatePaymentMethod(String id, PaymentMethodForm form) throws PaymentMethodException {
        log.info("Updating PaymentMethodForm by id: {}", id);

        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_PAYMENT_METHOD_ENTITY_ID.getValue(), id);
        Optional<PaymentMethodDocument> optActualDocument = repository.findById(id);
        if(!optActualDocument.isPresent()) {
            log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_NO_PAYMENT_METHOD_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_FOUND_PAYMENT_METHOD_ENTITY_ID.getValue(), id);

        PaymentMethodDocument actualDocument = optActualDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("PaymentMethodDocument is inactive with id: {}", id);
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("PaymentMethodDocument is active with id: {}", id);

        if(form == null) {
            log.debug("PaymentMethodForm is null");
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of PaymentMethodForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("PaymentMethodForm has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("PaymentMethodForm error detail: {}", ec);
            throw new PaymentMethodException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of PaymentMethodForm are empty");
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of PaymentMethodForm are valid");

        Optional<PaymentMethodDocument> optExpectedDocument = form2DocumentMapper.compareAndMap(actualDocument, form);
        if(!optExpectedDocument.isPresent()) {
            log.debug("No new value for attributes of PaymentMethodForm");
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from PaymentMethodForm to PaymentMethodDocument");

        PaymentMethodDocument expectedDocument = optExpectedDocument.get();

        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(actualDocument.getName().compareTo(expectedDocument.getName()) == 0
                || repository.existsByName(expectedDocument.getName())) {
            log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_EXISTS_BY_NAME.getValue(), expectedDocument.getName());
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_EXISTS,
                    new Object[]{ "name", actualDocument.getName() });
        }
        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_NON_EXISTENCE_BY_NAME.getValue(), expectedDocument.getName());

        entitySelfMapper.compareAndMap(expectedDocument, actualDocument);
        log.debug("Compared and copied attributes from PaymentMethodDocument to PaymentMethodForm");
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Updated: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to update {}", actualDocument);
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency paymentMethod details" });
        }
        log.info("Updated existing PaymentMethodDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void deletePaymentMethod(String id) throws PaymentMethodException {
        log.info("Soft deleting PaymentMethodDocument by id: {}", id);

        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_PAYMENT_METHOD_ENTITY_ID.getValue(), id);
        Optional<PaymentMethodDocument> optDocument = repository.findById(id);
        if(!optDocument.isPresent()) {
            log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_NO_PAYMENT_METHOD_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_FOUND_PAYMENT_METHOD_ENTITY_ID.getValue(), id);

        PaymentMethodDocument actualDocument = optDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("PaymentMethodDocument is inactive with id: {}", id);
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("PaymentMethodDocument is active with id: {}", id);

        actualDocument.setActive(Boolean.FALSE);
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualDocument);
        PaymentMethodDocument expectedDocument = repository.save(actualDocument);
        log.debug("Soft deleted: {}", expectedDocument);
        if(expectedDocument == null) {
            log.debug("Unable to soft delete {}", actualDocument);
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current paymentMethod details with id:" + id });
        }

        log.info("Soft deleted existing PaymentMethodDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void applyPatchOnPaymentMethod(String id, List<PatchOperationForm> patches) throws PaymentMethodException {
        log.info("Patching PaymentMethodDocument by id: {}", id);

        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_PAYMENT_METHOD_ENTITY_ID.getValue(), id);
        Optional<PaymentMethodDocument> optActualDocument = repository.findById(id);
        if(!optActualDocument.isPresent()) {
            log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_NO_PAYMENT_METHOD_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_FOUND_PAYMENT_METHOD_ENTITY_ID.getValue(), id);

        PaymentMethodDocument actualDocument = optActualDocument.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("PaymentMethod patch list not provided");
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("PaymentMethod patch list has {} items", patches.size());


        log.debug("Validating patch list items for PaymentMethod");
        try {
            toabBaseService.validatePatches(patches, SettingsErrorCode.SETTINGS_EXISTS.getDomain() + ":LOV");
            log.debug("All PaymentMethod patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the PaymentMethod patch item are invalid");
            throw new PaymentMethodException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for PaymentMethod");


        log.debug("Patching list items to PaymentMethodDto");
        PaymentMethodDto patchedPaymentMethodForm = new PaymentMethodDto();
        try {
            log.debug("Preparing patch list items for PaymentMethod");
            JsonNode paymentMethodDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch paymentMethodPatch = JsonPatch.fromJson(paymentMethodDtoTree);
            log.debug("Prepared patch list items for PaymentMethod");
            JsonNode blankPaymentMethodDtoTree = om.convertValue(new PaymentMethodDto(), JsonNode.class);
            JsonNode patchedPaymentMethodFormTree = paymentMethodPatch.apply(blankPaymentMethodDtoTree);
            log.debug("Applying patch list items to PaymentMethodDto");
            patchedPaymentMethodForm = om.treeToValue(patchedPaymentMethodFormTree, PaymentMethodDto.class);
            log.debug("Applied patch list items to PaymentMethodDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to PaymentMethodDto: {}", e);
            PaymentMethodException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in PaymentMethodDto");
                ex = new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new PaymentMethodException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to PaymentMethodDto: {}", e);
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to PaymentMethodDto");

        log.debug("Validating patched PaymentMethodDto");
        Errors err = new DirectFieldBindingResult(patchedPaymentMethodForm, patchedPaymentMethodForm.getClass().getSimpleName());
        dtoValidator.validate(patchedPaymentMethodForm, err);
        if(err.hasErrors()) {
            log.debug("Patched PaymentMethodDto has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched PaymentMethodDto error detail: {}", ec);
            throw new PaymentMethodException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched PaymentMethodDto are valid");

        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_EXISTENCE_BY_NAME.getValue(), patchedPaymentMethodForm.getName().get());
        if(actualDocument.getName().compareTo(patchedPaymentMethodForm.getName().get()) == 0
                || repository.existsByName(patchedPaymentMethodForm.getName().get())) {
            log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_EXISTS_BY_NAME.getValue(), patchedPaymentMethodForm.getName().get());
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_EXISTS,
                    new Object[]{ "name", patchedPaymentMethodForm.getName().get() });
        }
        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_NON_EXISTENCE_BY_NAME.getValue(), patchedPaymentMethodForm.getName().get());


        log.debug("Comparatively copying patched attributes from PaymentMethodDto to PaymentMethodDocument");
        try {
            dto2DocumentConverter.compareAndMap(patchedPaymentMethodForm, actualDocument);
        } catch (TOABBaseException e) {
            throw (PaymentMethodException) e;
        }
        log.debug("Comparatively copied patched attributes from PaymentMethodDto to PaymentMethodDocument");

        log.debug("Saving patched PaymentMethodDocument: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Saved patched PaymentMethodDocument: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to patch delete PaymentMethodDocument with id:{}", id);
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency paymentMethod details with id:" + id });
        }
        log.info("Patched PaymentMethodDocument with id:{}", id);
    }
}