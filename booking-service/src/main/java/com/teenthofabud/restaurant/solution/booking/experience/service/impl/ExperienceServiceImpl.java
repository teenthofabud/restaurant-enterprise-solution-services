package com.teenthofabud.restaurant.solution.booking.experience.service.impl;

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
import com.teenthofabud.restaurant.solution.booking.error.BookingErrorCode;
import com.teenthofabud.restaurant.solution.booking.experience.converter.ExperienceDto2DocumentConverter;
import com.teenthofabud.restaurant.solution.booking.experience.converter.ExperienceForm2DocumentConverter;
import com.teenthofabud.restaurant.solution.booking.experience.data.*;
import com.teenthofabud.restaurant.solution.booking.experience.mapper.ExperienceDocumentSelfMapper;
import com.teenthofabud.restaurant.solution.booking.experience.mapper.ExperienceForm2DocumentMapper;
import com.teenthofabud.restaurant.solution.booking.experience.repository.ExperienceRepository;
import com.teenthofabud.restaurant.solution.booking.experience.service.ExperienceService;
import com.teenthofabud.restaurant.solution.booking.experience.validator.ExperienceDtoValidator;
import com.teenthofabud.restaurant.solution.booking.experience.validator.ExperienceFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.booking.experience.validator.ExperienceFormValidator;
import com.teenthofabud.restaurant.solution.booking.utils.BookingServiceHelper;
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
public class ExperienceServiceImpl implements ExperienceService {

    private static final Comparator<ExperienceVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private ExperienceForm2DocumentConverter form2DocumentConverter;
    private ExperienceDto2DocumentConverter dto2DocumentConverter;
    private ExperienceForm2DocumentMapper form2DocumentMapper;
    private ExperienceDocumentSelfMapper documentSelfMapper;
    private ExperienceFormValidator formValidator;
    private ExperienceFormRelaxedValidator relaxedFormValidator;
    private ExperienceDtoValidator dtoValidator;
    private ExperienceRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private BookingServiceHelper bookingServiceHelper;

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }


    @Autowired
    public void setSessionServiceHelper(BookingServiceHelper bookingServiceHelper) {
        this.bookingServiceHelper = bookingServiceHelper;
    }

    @Autowired
    public void setDto2DocumentConverter(ExperienceDto2DocumentConverter dto2DocumentConverter) {
        this.dto2DocumentConverter = dto2DocumentConverter;
    }

    @Autowired
    public void setForm2DocumentMapper(ExperienceForm2DocumentMapper form2DocumentMapper) {
        this.form2DocumentMapper = form2DocumentMapper;
    }

    @Autowired
    public void setDocumentSelfMapper(ExperienceDocumentSelfMapper documentSelfMapper) {
        this.documentSelfMapper = documentSelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(ExperienceFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchExperienceValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(ExperienceDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2DocumentConverter(ExperienceForm2DocumentConverter form2DocumentConverter) {
        this.form2DocumentConverter = form2DocumentConverter;
    }

    @Autowired
    public void setRepository(ExperienceRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(ExperienceFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseExperienceId(String id) throws ExperienceException {
        Long experienceId = null;
        try {
            experienceId = Long.parseLong(id);
            log.debug("Parsed id {} to experience id {} in numeric format", id, experienceId);
            if(experienceId <= 0) {
                throw new NumberFormatException("experience id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse experience id", e);
            log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_EXPERIENCE_ID_INVALID.getValue(), id);
            throw new ExperienceException(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return experienceId;
    }

    @Override
    public Set<ExperienceVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all ExperienceDocument by their natural ordering");
        List<ExperienceDocument> experienceDocumentList = repository.findAll();
        List<ExperienceVo> experienceVoList = bookingServiceHelper.experienceDocument2DetailedVo(experienceDocumentList);
        Set<ExperienceVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_NAME);
        naturallyOrderedSet.addAll(experienceVoList);
        log.info("{} ExperienceVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public ExperienceVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws ExperienceException {
        log.info("Requesting ExperienceDocument by id: {}", id);
        Optional<ExperienceDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug("No ExperienceDocument found by id: {}", id);
            throw new ExperienceException(BookingErrorCode.BOOKING_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found ExperienceVo by id: {}", id);
        ExperienceDocument document = optDocument.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        ExperienceVo vo = bookingServiceHelper.experienceDocument2DetailedVo(document);
        log.debug("ExperienceVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<ExperienceVo> retrieveAllMatchingDetailsByCriteria(
            Optional<String> optionalName, Optional<String> optionalDescription) throws ExperienceException {
        if(optionalName.isEmpty() && optionalDescription.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String description = optionalDescription.isPresent() ? optionalDescription.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(description))) {
            log.debug("All search parameters are empty");
        }
        List<ExperienceVo> matchedExperienceList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        ExperienceDocument document = new ExperienceDocument();
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
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<ExperienceDocument> experienceDocumentExample = Example.of(document, matcherCriteria);
        List<ExperienceDocument> experienceDocumentList = repository.findAll(experienceDocumentExample);
        matchedExperienceList = bookingServiceHelper.experienceDocument2DetailedVo(experienceDocumentList);
        log.info("Found {} ExperienceVo matching with provided parameters : {}", matchedExperienceList.size(), providedFilters);
        log.info("No ExperienceVo available matching with provided parameters : {}", matchedExperienceList.size(), providedFilters);
        return matchedExperienceList;
    }

    @Override
    public String createExperience(ExperienceForm form) throws ExperienceException {
        log.info("Creating new ExperienceDocument");

        if(form == null) {
            log.debug("ExperienceForm provided is null");
            throw new ExperienceException(BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of ExperienceForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("ExperienceForm has {} errors", err.getErrorCount());
            BookingErrorCode ec = BookingErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("ExperienceForm error detail: {}", ec);
            throw new ExperienceException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of ExperienceForm are valid");

        ExperienceDocument expectedDocument = form2DocumentConverter.convert(form);

        log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_EXPERIENCE_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(repository.existsByName(expectedDocument.getName())) {
            log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_EXPERIENCE_EXISTS_BY_NAME.getValue(), expectedDocument.getName());
            throw new ExperienceException(BookingErrorCode.BOOKING_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_EXPERIENCE_NON_EXISTENCE_BY_NAME.getValue(), expectedDocument.getName());

        log.debug("Saving {}", expectedDocument);
        ExperienceDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null) {
            log.debug("Unable to create {}", expectedDocument);
            throw new ExperienceException(BookingErrorCode.BOOKING_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist ExperienceForm details" });
        }
        log.info("Created new ExperienceForm with id: {}", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Override
    public void updateExperience(String id, ExperienceForm form) throws ExperienceException {
        log.info("Updating ExperienceForm by id: {}", id);

        log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_EXPERIENCE_ENTITY_ID.getValue(), id);
        Optional<ExperienceDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_NO_EXPERIENCE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ExperienceException(BookingErrorCode.BOOKING_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_FOUND_EXPERIENCE_ENTITY_ID.getValue(), id);

        ExperienceDocument actualDocument = optActualDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("ExperienceDocument is inactive with id: {}", id);
            throw new ExperienceException(BookingErrorCode.BOOKING_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("ExperienceDocument is active with id: {}", id);

        if(form == null) {
            log.debug("ExperienceForm is null");
            throw new ExperienceException(BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of ExperienceForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("ExperienceForm has {} errors", err.getErrorCount());
            BookingErrorCode ec = BookingErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("ExperienceForm error detail: {}", ec);
            throw new ExperienceException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of ExperienceForm are empty");
            throw new ExperienceException(BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of ExperienceForm are valid");

        Optional<ExperienceDocument> optExpectedDocument = form2DocumentMapper.compareAndMap(actualDocument, form);
        if(optExpectedDocument.isEmpty()) {
            log.debug("No new value for attributes of ExperienceForm");
            throw new ExperienceException(BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from ExperienceForm to ExperienceDocument");

        ExperienceDocument expectedDocument = optExpectedDocument.get();

        log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_EXPERIENCE_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(actualDocument.getName().compareTo(expectedDocument.getName()) == 0
                || repository.existsByName(expectedDocument.getName())) {
            log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_EXPERIENCE_EXISTS_BY_NAME.getValue(), expectedDocument.getName());
            throw new ExperienceException(BookingErrorCode.BOOKING_EXISTS,
                    new Object[]{ "name", actualDocument.getName() });
        }
        log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_EXPERIENCE_NON_EXISTENCE_BY_NAME.getValue(), expectedDocument.getName());

        documentSelfMapper.compareAndMap(expectedDocument, actualDocument);
        log.debug("Compared and copied attributes from ExperienceDocument to ExperienceForm");
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Updated: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to update {}", actualDocument);
            throw new ExperienceException(BookingErrorCode.BOOKING_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency experience details" });
        }
        log.info("Updated existing ExperienceDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void deleteExperience(String id) throws ExperienceException {
        log.info("Soft deleting ExperienceDocument by id: {}", id);

        log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_EXPERIENCE_ENTITY_ID.getValue(), id);
        Optional<ExperienceDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_NO_EXPERIENCE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ExperienceException(BookingErrorCode.BOOKING_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_FOUND_EXPERIENCE_ENTITY_ID.getValue(), id);

        ExperienceDocument actualDocument = optDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("ExperienceDocument is inactive with id: {}", id);
            throw new ExperienceException(BookingErrorCode.BOOKING_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("ExperienceDocument is active with id: {}", id);

        actualDocument.setActive(Boolean.FALSE);
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualDocument);
        ExperienceDocument expectedDocument = repository.save(actualDocument);
        log.debug("Soft deleted: {}", expectedDocument);
        if(expectedDocument == null) {
            log.debug("Unable to soft delete {}", actualDocument);
            throw new ExperienceException(BookingErrorCode.BOOKING_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current experience details with id:" + id });
        }

        log.info("Soft deleted existing ExperienceDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void applyPatchOnExperience(String id, List<PatchOperationForm> patches) throws ExperienceException {
        log.info("Patching ExperienceDocument by id: {}", id);

        log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_EXPERIENCE_ENTITY_ID.getValue(), id);
        Optional<ExperienceDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_NO_EXPERIENCE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ExperienceException(BookingErrorCode.BOOKING_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_FOUND_EXPERIENCE_ENTITY_ID.getValue(), id);

        ExperienceDocument actualDocument = optActualDocument.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Experience patch list not provided");
            throw new ExperienceException(BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Experience patch list has {} items", patches.size());


        log.debug("Validating patch list items for Experience");
        try {
            toabBaseService.validatePatches(patches, BookingErrorCode.BOOKING_EXISTS.getDomain() + ":LOV");
            log.debug("All Experience patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Experience patch item are invalid");
            throw new ExperienceException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Experience");


        log.debug("Patching list items to ExperienceDto");
        ExperienceDto patchedExperienceForm = new ExperienceDto();
        try {
            log.debug("Preparing patch list items for Experience");
            JsonNode experienceDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch experiencePatch = JsonPatch.fromJson(experienceDtoTree);
            log.debug("Prepared patch list items for Experience");
            JsonNode blankExperienceDtoTree = om.convertValue(new ExperienceDto(), JsonNode.class);
            JsonNode patchedExperienceFormTree = experiencePatch.apply(blankExperienceDtoTree);
            log.debug("Applying patch list items to ExperienceDto");
            patchedExperienceForm = om.treeToValue(patchedExperienceFormTree, ExperienceDto.class);
            log.debug("Applied patch list items to ExperienceDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to ExperienceDto: {}", e);
            ExperienceException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in ExperienceDto");
                ex = new ExperienceException(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new ExperienceException(BookingErrorCode.BOOKING_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to ExperienceDto: {}", e);
            throw new ExperienceException(BookingErrorCode.BOOKING_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to ExperienceDto");

        log.debug("Validating patched ExperienceDto");
        Errors err = new DirectFieldBindingResult(patchedExperienceForm, patchedExperienceForm.getClass().getSimpleName());
        dtoValidator.validate(patchedExperienceForm, err);
        if(err.hasErrors()) {
            log.debug("Patched ExperienceDto has {} errors", err.getErrorCount());
            BookingErrorCode ec = BookingErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched ExperienceDto error detail: {}", ec);
            throw new ExperienceException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched ExperienceDto are valid");

        log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_EXPERIENCE_EXISTENCE_BY_NAME.getValue(), patchedExperienceForm.getName().get());
        if(actualDocument.getName().compareTo(patchedExperienceForm.getName().get()) == 0
                || repository.existsByName(patchedExperienceForm.getName().get())) {
            log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_EXPERIENCE_EXISTS_BY_NAME.getValue(), patchedExperienceForm.getName().get());
            throw new ExperienceException(BookingErrorCode.BOOKING_EXISTS,
                    new Object[]{ "name", patchedExperienceForm.getName().get() });
        }
        log.debug(ExperienceMessageTemplate.MSG_TEMPLATE_EXPERIENCE_NON_EXISTENCE_BY_NAME.getValue(), patchedExperienceForm.getName().get());


        log.debug("Comparatively copying patched attributes from ExperienceDto to ExperienceDocument");
        try {
            dto2DocumentConverter.compareAndMap(patchedExperienceForm, actualDocument);
        } catch (TOABBaseException e) {
            throw (ExperienceException) e;
        }
        log.debug("Comparatively copied patched attributes from ExperienceDto to ExperienceDocument");

        log.debug("Saving patched ExperienceDocument: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Saved patched ExperienceDocument: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to patch delete ExperienceDocument with id:{}", id);
            throw new ExperienceException(BookingErrorCode.BOOKING_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency experience details with id:" + id });
        }
        log.info("Patched ExperienceDocument with id:{}", id);
    }
}