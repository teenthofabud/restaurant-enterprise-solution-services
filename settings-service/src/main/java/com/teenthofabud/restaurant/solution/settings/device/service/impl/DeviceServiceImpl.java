package com.teenthofabud.restaurant.solution.settings.device.service.impl;

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
import com.teenthofabud.restaurant.solution.settings.device.converter.DeviceDto2DocumentConverter;
import com.teenthofabud.restaurant.solution.settings.device.converter.DeviceForm2DocumentConverter;
import com.teenthofabud.restaurant.solution.settings.device.data.*;
import com.teenthofabud.restaurant.solution.settings.device.mapper.DeviceDocumentSelfMapper;
import com.teenthofabud.restaurant.solution.settings.device.mapper.DeviceForm2DocumentMapper;
import com.teenthofabud.restaurant.solution.settings.device.repository.DeviceRepository;
import com.teenthofabud.restaurant.solution.settings.device.service.DeviceService;
import com.teenthofabud.restaurant.solution.settings.device.validator.DeviceDtoValidator;
import com.teenthofabud.restaurant.solution.settings.device.validator.DeviceFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.settings.device.validator.DeviceFormValidator;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
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
public class DeviceServiceImpl implements DeviceService {

    private static final Comparator<DeviceVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private DeviceForm2DocumentConverter form2DocumentConverter;
    private DeviceDto2DocumentConverter dto2DocumentConverter;
    private DeviceForm2DocumentMapper form2DocumentMapper;
    private DeviceDocumentSelfMapper documentSelfMapper;
    private DeviceFormValidator formValidator;
    private DeviceFormRelaxedValidator relaxedFormValidator;
    private DeviceDtoValidator dtoValidator;
    private DeviceRepository repository;
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
    public void setDto2DocumentConverter(DeviceDto2DocumentConverter dto2DocumentConverter) {
        this.dto2DocumentConverter = dto2DocumentConverter;
    }

    @Autowired
    public void setForm2DocumentMapper(DeviceForm2DocumentMapper form2DocumentMapper) {
        this.form2DocumentMapper = form2DocumentMapper;
    }

    @Autowired
    public void setDocumentSelfMapper(DeviceDocumentSelfMapper documentSelfMapper) {
        this.documentSelfMapper = documentSelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(DeviceFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchDeviceValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(DeviceDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2DocumentConverter(DeviceForm2DocumentConverter form2DocumentConverter) {
        this.form2DocumentConverter = form2DocumentConverter;
    }

    @Autowired
    public void setRepository(DeviceRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(DeviceFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseDeviceId(String id) throws DeviceException {
        Long deviceId = null;
        try {
            deviceId = Long.parseLong(id);
            log.debug("Parsed id {} to device id {} in numeric format", id, deviceId);
            if(deviceId <= 0) {
                throw new NumberFormatException("device id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse device id", e);
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_ID_INVALID.getValue(), id);
            throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return deviceId;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public Set<DeviceVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all DeviceDocument by their natural ordering");
        List<DeviceDocument> deviceDocumentList = repository.findAll();
        List<DeviceVo> deviceVoList = settingsServiceHelper.deviceDocument2DetailedVo(deviceDocumentList);
        Set<DeviceVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_NAME);
        naturallyOrderedSet.addAll(deviceVoList);
        log.info("{} DeviceVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public DeviceVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws DeviceException {
        log.info("Requesting DeviceDocument by id: {}", id);
        Optional<DeviceDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug("No DeviceDocument found by id: {}", id);
            throw new DeviceException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found DeviceVo by id: {}", id);
        DeviceDocument document = optDocument.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        DeviceVo vo = settingsServiceHelper.deviceDocument2DetailedVo(document);
        log.debug("DeviceVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<DeviceVo> retrieveAllMatchingDetailsByCriteria(
            Optional<String> optionalName, Optional<String> optionalDescription, Optional<String> optionalDeviceTypeId) throws DeviceException {
        if(optionalName.isEmpty() && optionalDescription.isEmpty() && optionalDeviceTypeId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String description = optionalDescription.isPresent() ? optionalDescription.get() : "";
        String deviceTypeId = optionalDeviceTypeId.isPresent() ? optionalDeviceTypeId.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(description))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(deviceTypeId))) {
            log.debug("All search parameters are empty");
        }
        List<DeviceVo> matchedDeviceList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        DeviceDocument document = new DeviceDocument();
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
        if(StringUtils.hasText(StringUtils.trimWhitespace(deviceTypeId))) {
            try {
                DeviceType.valueOf(deviceTypeId);
            } catch (IllegalArgumentException e) {
                log.error("deviceTypeId parameter is invalid", e);
                throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "deviceTypeId", deviceTypeId });
            }
            log.debug("deviceTypeId {} is valid", description);
            providedFilters.put("deviceTypeId", description);
            document.setDeviceTypeId(deviceTypeId);
            matcherCriteria = matcherCriteria.withMatcher("deviceTypeId", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<DeviceDocument> deviceDocumentExample = Example.of(document, matcherCriteria);
        List<DeviceDocument> deviceDocumentList = repository.findAll(deviceDocumentExample);
        matchedDeviceList = settingsServiceHelper.deviceDocument2DetailedVo(deviceDocumentList);
        log.info("Found {} DeviceVo matching with provided parameters : {}", matchedDeviceList.size(), providedFilters);
        log.info("No DeviceVo available matching with provided parameters : {}", matchedDeviceList.size(), providedFilters);
        return matchedDeviceList;
    }

    @Transactional
    @Override
    public String createDevice(DeviceForm form) throws DeviceException {
        log.info("Creating new DeviceDocument");

        if(form == null) {
            log.debug("DeviceForm provided is null");
            throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of DeviceForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("DeviceForm has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("DeviceForm error detail: {}", ec);
            throw new DeviceException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of DeviceForm are valid");

        DeviceDocument expectedDocument = form2DocumentConverter.convert(form);

        log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(), form.getName(), form.getDeviceTypeId());
        if(repository.existsByNameAndDeviceTypeId(expectedDocument.getName(), expectedDocument.getDeviceTypeId())) {
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(), expectedDocument.getName());
            throw new DeviceException(SettingsErrorCode.SETTINGS_EXISTS,
                    new Object[]{ "name: " + form.getName(), ", deviceTypeId: " + form.getDeviceTypeId() });
        }
        log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(), expectedDocument.getName());

        log.debug("Saving {}", expectedDocument);
        DeviceDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null) {
            log.debug("Unable to create {}", expectedDocument);
            throw new DeviceException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist DeviceForm details" });
        }
        log.info("Created new DeviceForm with id: {}", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Transactional
    @Override
    public void updateDevice(String id, DeviceForm form) throws DeviceException {
        log.info("Updating DeviceForm by id: {}", id);

        log.debug(DeviceMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_DEVICE_DOCUMENT_ID.getValue(), id);
        Optional<DeviceDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_NO_DEVICE_DOCUMENT_ID_AVAILABLE.getValue(), id);
            throw new DeviceException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(DeviceMessageTemplate.MSG_TEMPLATE_FOUND_DEVICE_DOCUMENT_ID.getValue(), id);

        DeviceDocument actualDocument = optActualDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("DeviceDocument is inactive with id: {}", id);
            throw new DeviceException(SettingsErrorCode.SETTINGS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("DeviceDocument is active with id: {}", id);

        if(form == null) {
            log.debug("DeviceForm is null");
            throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of DeviceForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("DeviceForm has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("DeviceForm error detail: {}", ec);
            throw new DeviceException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of DeviceForm are empty");
            throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of DeviceForm are valid");

        Optional<DeviceDocument> optExpectedDocument = form2DocumentMapper.compareAndMap(actualDocument, form);
        if(optExpectedDocument.isEmpty()) {
            log.debug("No new value for attributes of DeviceForm");
            throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from DeviceForm to DeviceDocument");

        DeviceDocument expectedDocument = optExpectedDocument.get();

        checkUniquenessOfDevice(form, actualDocument);

        documentSelfMapper.compareAndMap(expectedDocument, actualDocument);
        log.debug("Compared and copied attributes from DeviceDocument to DeviceForm");
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Updated: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to update {}", actualDocument);
            throw new DeviceException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency device details" });
        }
        log.info("Updated existing DeviceDocument with id: {}", actualDocument.getId());
    }

    @Transactional
    @Override
    public void deleteDevice(String id) throws DeviceException {
        log.info("Soft deleting DeviceDocument by id: {}", id);

        log.debug(DeviceMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_DEVICE_DOCUMENT_ID.getValue(), id);
        Optional<DeviceDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_NO_DEVICE_DOCUMENT_ID_AVAILABLE.getValue(), id);
            throw new DeviceException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(DeviceMessageTemplate.MSG_TEMPLATE_FOUND_DEVICE_DOCUMENT_ID.getValue(), id);

        DeviceDocument actualDocument = optDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("DeviceDocument is inactive with id: {}", id);
            throw new DeviceException(SettingsErrorCode.SETTINGS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("DeviceDocument is active with id: {}", id);

        actualDocument.setActive(Boolean.FALSE);
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualDocument);
        DeviceDocument expectedDocument = repository.save(actualDocument);
        log.debug("Soft deleted: {}", expectedDocument);
        if(expectedDocument == null) {
            log.debug("Unable to soft delete {}", actualDocument);
            throw new DeviceException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current device details with id:" + id });
        }

        log.info("Soft deleted existing DeviceDocument with id: {}", actualDocument.getId());
    }

    @Transactional
    @Override
    public void applyPatchOnDevice(String id, List<PatchOperationForm> patches) throws DeviceException {
        log.info("Patching DeviceDocument by id: {}", id);

        log.debug(DeviceMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_DEVICE_DOCUMENT_ID.getValue(), id);
        Optional<DeviceDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_NO_DEVICE_DOCUMENT_ID_AVAILABLE.getValue(), id);
            throw new DeviceException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(DeviceMessageTemplate.MSG_TEMPLATE_FOUND_DEVICE_DOCUMENT_ID.getValue(), id);

        DeviceDocument actualDocument = optActualDocument.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Device patch list not provided");
            throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Device patch list has {} items", patches.size());


        log.debug("Validating patch list items for Device");
        try {
            toabBaseService.validatePatches(patches, SettingsErrorCode.SETTINGS_EXISTS.getDomain() + ":LOV");
            log.debug("All Device patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Device patch item are invalid");
            throw new DeviceException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Device");


        log.debug("Patching list items to DeviceDto");
        DeviceDto patchedDeviceForm = new DeviceDto();
        try {
            log.debug("Preparing patch list items for Device");
            JsonNode deviceDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch devicePatch = JsonPatch.fromJson(deviceDtoTree);
            log.debug("Prepared patch list items for Device");
            JsonNode blankDeviceDtoTree = om.convertValue(new DeviceDto(), JsonNode.class);
            JsonNode patchedDeviceFormTree = devicePatch.apply(blankDeviceDtoTree);
            log.debug("Applying patch list items to DeviceDto");
            patchedDeviceForm = om.treeToValue(patchedDeviceFormTree, DeviceDto.class);
            log.debug("Applied patch list items to DeviceDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to DeviceDto: {}", e);
            DeviceException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in DeviceDto");
                ex = new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new DeviceException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to DeviceDto: {}", e);
            throw new DeviceException(SettingsErrorCode.SETTINGS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to DeviceDto");

        log.debug("Validating patched DeviceDto");
        Errors err = new DirectFieldBindingResult(patchedDeviceForm, patchedDeviceForm.getClass().getSimpleName());
        dtoValidator.validate(patchedDeviceForm, err);
        if(err.hasErrors()) {
            log.debug("Patched DeviceDto has {} errors", err.getErrorCount());
            SettingsErrorCode ec = SettingsErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched DeviceDto error detail: {}", ec);
            throw new DeviceException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched DeviceDto are valid");

        checkUniquenessOfDevice(patchedDeviceForm, actualDocument);

        log.debug("Comparatively copying patched attributes from DeviceDto to DeviceDocument");
        try {
            dto2DocumentConverter.compareAndMap(patchedDeviceForm, actualDocument);
        } catch (TOABBaseException e) {
            throw (DeviceException) e;
        }
        log.debug("Comparatively copied patched attributes from DeviceDto to DeviceDocument");

        log.debug("Saving patched DeviceDocument: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Saved patched DeviceDocument: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to patch delete DeviceDocument with id:{}", id);
            throw new DeviceException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency device details with id:" + id });
        }
        log.info("Patched DeviceDocument with id:{}", id);
    }

    private void checkUniquenessOfDevice(DeviceDto patchedDeviceForm, DeviceDocument actualDocument) throws DeviceException {
        // name = true, deviceTypeId = false
        if(patchedDeviceForm.getName().isPresent() && patchedDeviceForm.getDeviceTypeId().isEmpty()) {
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    patchedDeviceForm.getName().get(), actualDocument.getDeviceTypeId());
            boolean sameDocumentSw = patchedDeviceForm.getName().get().compareTo(actualDocument.getName()) == 0;
            boolean duplicateDocumentSw =  repository.existsByNameAndDeviceTypeId(patchedDeviceForm.getName().get(), actualDocument.getDeviceTypeId());
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        patchedDeviceForm.getName().get(), actualDocument.getDeviceTypeId());
                throw new DeviceException(SettingsErrorCode.SETTINGS_EXISTS,
                        new Object[]{ "name: " + patchedDeviceForm.getName().get(), ", deviceTypeId: " + actualDocument.getDeviceTypeId() });
            }
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    patchedDeviceForm.getName().get(), actualDocument.getDeviceTypeId());
        }

        // name = true, deviceTypeId = true
        if(patchedDeviceForm.getName().isPresent() && patchedDeviceForm.getDeviceTypeId().isPresent()) {
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    patchedDeviceForm.getName().get(), patchedDeviceForm.getDeviceTypeId().get());
            boolean sameDocumentSw = patchedDeviceForm.getName().get().compareTo(actualDocument.getName()) == 0
                    && patchedDeviceForm.getDeviceTypeId().get().compareTo(actualDocument.getDeviceTypeId()) == 0;
            boolean duplicateDocumentSw =  repository.existsByNameAndDeviceTypeId(patchedDeviceForm.getName().get(),
                    patchedDeviceForm.getDeviceTypeId().get());
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        patchedDeviceForm.getName().get(), patchedDeviceForm.getDeviceTypeId().get());
                throw new DeviceException(SettingsErrorCode.SETTINGS_EXISTS,
                        new Object[]{ "name: " + patchedDeviceForm.getName().get(), ", deviceTypeId: " + patchedDeviceForm.getDeviceTypeId().get() });
            }
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    patchedDeviceForm.getName().get(), patchedDeviceForm.getDeviceTypeId().get());
        }

        // name = false, deviceTypeId = true
        if(patchedDeviceForm.getName().isEmpty() && patchedDeviceForm.getDeviceTypeId().isPresent()) {
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    actualDocument.getName(),  patchedDeviceForm.getDeviceTypeId().get());
            boolean sameDocumentSw = patchedDeviceForm.getDeviceTypeId().get().compareTo(actualDocument.getDeviceTypeId()) == 0;
            boolean duplicateDocumentSw =  repository.existsByNameAndDeviceTypeId(
                    actualDocument.getName(), patchedDeviceForm.getDeviceTypeId().get());
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        actualDocument.getName(), patchedDeviceForm.getDeviceTypeId().get());
                throw new DeviceException(SettingsErrorCode.SETTINGS_EXISTS, new Object[]{ "name " + actualDocument.getName(),
                        ", deviceTypeId: " + patchedDeviceForm.getDeviceTypeId().get() });
            }
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    actualDocument.getName(),  patchedDeviceForm.getDeviceTypeId().get());
        }
    }

    private void checkUniquenessOfDevice(DeviceForm deviceForm, DeviceDocument actualDocument) throws DeviceException {
        // name = true, deviceTypeId = false
        if(StringUtils.hasText(StringUtils.trimWhitespace(deviceForm.getName()))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(deviceForm.getDeviceTypeId()))) {
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    deviceForm.getName(), actualDocument.getDeviceTypeId());
            boolean sameDocumentSw = deviceForm.getName().compareTo(actualDocument.getName()) == 0;
            boolean duplicateDocumentSw =  repository.existsByNameAndDeviceTypeId(deviceForm.getName(), actualDocument.getDeviceTypeId());
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        deviceForm.getName(), actualDocument.getDeviceTypeId());
                throw new DeviceException(SettingsErrorCode.SETTINGS_EXISTS,
                        new Object[]{ "name: " + deviceForm.getName(), ", deviceTypeId: " + actualDocument.getDeviceTypeId() });
            }
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    deviceForm.getName(), actualDocument.getDeviceTypeId());
        }

        // name = true, deviceTypeId = true
        if(StringUtils.hasText(StringUtils.trimWhitespace(deviceForm.getName()))
                && StringUtils.hasText(StringUtils.trimWhitespace(deviceForm.getDeviceTypeId()))) {
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    deviceForm.getName(), deviceForm.getDeviceTypeId());
            boolean sameDocumentSw = deviceForm.getName().compareTo(actualDocument.getName()) == 0
                    && deviceForm.getDeviceTypeId().compareTo(actualDocument.getDeviceTypeId()) == 0;
            boolean duplicateDocumentSw =  repository.existsByNameAndDeviceTypeId(deviceForm.getName(), deviceForm.getDeviceTypeId());
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        deviceForm.getName(), deviceForm.getDeviceTypeId());
                throw new DeviceException(SettingsErrorCode.SETTINGS_EXISTS,
                        new Object[]{ "name: " + deviceForm.getName(), ", deviceTypeId: " + deviceForm.getDeviceTypeId() });
            }
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    deviceForm.getName(), deviceForm.getDeviceTypeId());
        }

        // name = false, deviceTypeId = true
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(deviceForm.getName()))
                && StringUtils.hasText(StringUtils.trimWhitespace(deviceForm.getDeviceTypeId()))) {
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    actualDocument.getName(),  deviceForm.getDeviceTypeId());
            boolean sameDocumentSw = deviceForm.getName().compareTo(actualDocument.getName()) == 0
                    && deviceForm.getDeviceTypeId().compareTo(actualDocument.getDeviceTypeId()) == 0;
            boolean duplicateDocumentSw =  repository.existsByNameAndDeviceTypeId(actualDocument.getName(),
                    deviceForm.getDeviceTypeId());
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_EXISTS_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                        actualDocument.getName(),  deviceForm.getDeviceTypeId());
                throw new DeviceException(SettingsErrorCode.SETTINGS_EXISTS, new Object[]{ "name: " + actualDocument.getName(),
                        ", deviceTypeId: " + deviceForm.getDeviceTypeId() });
            }
            log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_NON_EXISTENCE_BY_NAME_AND_TEMPLATE_TYPE_ID.getValue(),
                    actualDocument.getName(),  deviceForm.getDeviceTypeId());
        }
    }
    
}