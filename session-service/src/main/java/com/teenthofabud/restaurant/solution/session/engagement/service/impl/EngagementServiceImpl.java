package com.teenthofabud.restaurant.solution.session.engagement.service.impl;

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
import com.teenthofabud.restaurant.solution.session.engagement.converter.EngagementDto2DocumentConverter;
import com.teenthofabud.restaurant.solution.session.engagement.converter.EngagementForm2DocumentConverter;
import com.teenthofabud.restaurant.solution.session.engagement.data.*;
import com.teenthofabud.restaurant.solution.session.engagement.mapper.EngagementDocumentSelfMapper;
import com.teenthofabud.restaurant.solution.session.engagement.mapper.EngagementForm2DocumentMapper;
import com.teenthofabud.restaurant.solution.session.engagement.repository.EngagementRepository;
import com.teenthofabud.restaurant.solution.session.engagement.service.EngagementService;
import com.teenthofabud.restaurant.solution.session.engagement.validator.EngagementDtoValidator;
import com.teenthofabud.restaurant.solution.session.engagement.validator.EngagementFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.session.engagement.validator.EngagementFormValidator;
import com.teenthofabud.restaurant.solution.session.error.SessionErrorCode;
import com.teenthofabud.restaurant.solution.session.association.data.AssociationException;
import com.teenthofabud.restaurant.solution.session.association.data.AssociationVo;
import com.teenthofabud.restaurant.solution.session.association.service.AssociationService;
import com.teenthofabud.restaurant.solution.session.utils.SessionServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.*;

@Component
@Slf4j
public class EngagementServiceImpl implements EngagementService {

    private static final Comparator<EngagementVo> CMP_BY_ASSOCIATION_DATE_TIME_EVENT = (s1, s2) -> {
       int c1 = (s1.getAssociation() == null || s2.getAssociation() == null) ? s1.getAssociationId().compareTo(s2.getAssociationId())
               : s1.getAssociation().compareTo(s2.getAssociation());
       if(c1 == 0) {
           int c2 = s1.getDate().compareTo(s2.getDate());
           if(c2 == 0) {
               int c3 = s1.getTime().compareTo(s2.getTime());
               if(c3 == 0) {
                   int c4 = s1.getEvent().compareTo(s2.getEvent());
                   return c4;
               } else {
                   return c3;
               }
           } else {
               return c2;
           }
       } else {
           return c1;
       }
    };

    private EngagementForm2DocumentConverter form2DocumentConverter;
    private EngagementDto2DocumentConverter dto2DocumentConverter;
    private EngagementForm2DocumentMapper form2DocumentMapper;
    private EngagementDocumentSelfMapper documentSelfMapper;
    private EngagementFormValidator formValidator;
    private EngagementFormRelaxedValidator relaxedFormValidator;
    private EngagementDtoValidator dtoValidator;
    private EngagementRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private SessionServiceHelper sessionServiceHelper;
    private AssociationService associationService;

    private String dateFormat;
    private String timeFormat;
    private String timestampFormat;

    @Value("${res.session.engagement.timestamp.format}")
    public void setTimestampFormat(String timestampFormat) {
        this.timestampFormat = timestampFormat;
    }

    @Value("${res.session.engagement.date.format}")
    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    @Value("${res.session.engagement.time.format}")
    public void setTimeFormat(String timeFormat) {
        this.timeFormat = timeFormat;
    }

    @Autowired
    public void setAssociationService(AssociationService associationService) {
        this.associationService = associationService;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }


    @Autowired
    public void setSessionServiceHelper(SessionServiceHelper sessionServiceHelper) {
        this.sessionServiceHelper = sessionServiceHelper;
    }

    @Autowired
    public void setDto2DocumentConverter(EngagementDto2DocumentConverter dto2DocumentConverter) {
        this.dto2DocumentConverter = dto2DocumentConverter;
    }

    @Autowired
    public void setForm2DocumentMapper(EngagementForm2DocumentMapper form2DocumentMapper) {
        this.form2DocumentMapper = form2DocumentMapper;
    }

    @Autowired
    public void setDocumentSelfMapper(EngagementDocumentSelfMapper documentSelfMapper) {
        this.documentSelfMapper = documentSelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(EngagementFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchEngagementValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(EngagementDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2DocumentConverter(EngagementForm2DocumentConverter form2DocumentConverter) {
        this.form2DocumentConverter = form2DocumentConverter;
    }

    @Autowired
    public void setRepository(EngagementRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(EngagementFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseEngagementId(String id) throws EngagementException {
        Long engagementId = null;
        try {
            engagementId = Long.parseLong(id);
            log.debug("Parsed id {} to engagement id {} in numeric format", id, engagementId);
            if(engagementId <= 0) {
                throw new NumberFormatException("engagement id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse engagement id", e);
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_ID_INVALID.getValue(), id);
            throw new EngagementException(SessionErrorCode.SESSION_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return engagementId;
    }

    @Override
    public Set<EngagementVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all EngagementDocument by their natural ordering");
        List<EngagementDocument> engagementDocumentList = repository.findAll();
        List<EngagementVo> engagementVoList = sessionServiceHelper.engagementDocument2DetailedVo(engagementDocumentList);
        Set<EngagementVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_ASSOCIATION_DATE_TIME_EVENT);
        naturallyOrderedSet.addAll(engagementVoList);
        log.info("{} EngagementVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public EngagementVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws EngagementException {
        log.info("Requesting EngagementDocument by id: {}", id);
        Optional<EngagementDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug("No EngagementDocument found by id: {}", id);
            throw new EngagementException(SessionErrorCode.SESSION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found EngagementVo by id: {}", id);
        EngagementDocument document = optDocument.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        EngagementVo vo = sessionServiceHelper.engagementDocument2DetailedVo(document);
        log.debug("EngagementVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<EngagementVo> retrieveAllMatchingDetailsByAssociationId(String associationId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws EngagementException {
        log.info("Requesting EngagementDocument that match with associationId: {}", associationId);
        Errors err = new DirectFieldBindingResult(associationId, "EngagementForm");
        try {
            AssociationVo associationVo = associationService.retrieveDetailsById(associationId, Optional.of(TOABCascadeLevel.ONE));
            if(!associationVo.getActive()) {
                throw new AssociationException(SessionErrorCode.SESSION_INACTIVE, new Object [] { associationId });
            }
        } catch (AssociationException e) {
            log.error("associationId is invalid", e);
            throw new EngagementException(SessionErrorCode.SESSION_ATTRIBUTE_INVALID, new Object [] { "associationId: " + associationId });
        }
        List<EngagementDocument> engagementDocumentList = repository.findByAssociationId(associationId);
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        List<EngagementVo> matchedEngagementList = sessionServiceHelper.engagementDocument2DetailedVo(engagementDocumentList);
        log.info("Found {} EngagementVo matching with recipeId: {}", matchedEngagementList.size(), associationId);
        TOABRequestContextHolder.clearCascadeLevelContext();
        if(matchedEngagementList.isEmpty()) {
            log.debug("No EngagementVo found matching with associationId: {}", associationId);
            throw new EngagementException(SessionErrorCode.SESSION_NOT_FOUND, new Object[] { "associationId", associationId });
        }
        return matchedEngagementList;
    }

    @Override
    public List<EngagementVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalAssociationId, Optional<String> optionalEvent, Optional<String> optionalTimestamp) throws EngagementException {
        if(optionalAssociationId.isEmpty() && optionalEvent.isEmpty() && optionalTimestamp.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String associationId = optionalAssociationId.isPresent() ? optionalAssociationId.get() : "";
        String event = optionalEvent.isPresent() ? optionalEvent.get() : "";
        String timestamp = optionalTimestamp.isPresent() ? optionalTimestamp.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(associationId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(event))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(timestamp))) {
            log.debug("All search parameters are empty");
        }
        List<EngagementVo> matchedEngagementList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        EngagementDocument document = new EngagementDocument();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(associationId))) {
            log.debug("associationId {} is valid", associationId);
            providedFilters.put("associationId", associationId);
            document.setAssociationId(associationId);
            matcherCriteria = matcherCriteria.withMatcher("associationId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(event))) {
            log.debug("event {} is valid", event);
            providedFilters.put("event", event);
            document.setEvent(EngagementEvent.valueOf(event));
            matcherCriteria = matcherCriteria.withMatcher("event", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(timestamp))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(timestamp);
                LocalDateTime timestampRaw = LocalDateTime.parse(timestamp, dtf);
                log.debug("timestamp {} is valid", timestamp);
                providedFilters.put("timestamp", timestamp);
                document.setDate(timestampRaw.toLocalDate());
                document.setTime(timestampRaw.toLocalTime());
                matcherCriteria = matcherCriteria.withMatcher("timestamp", match -> match.exact());
            } catch (DateTimeParseException e) {
                log.error("Unable to parse timestamp", e);
                log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_TIMESTAMP_INVALID.getValue(), timestamp);
                throw new EngagementException(SessionErrorCode.SESSION_ATTRIBUTE_INVALID, new Object[] { "timestamp", timestamp });
            } 
            log.debug("event {} is valid", event);
            providedFilters.put("event", event);
            document.setEvent(EngagementEvent.valueOf(event));
            matcherCriteria = matcherCriteria.withMatcher("event", match -> match.exact());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<EngagementDocument> engagementDocumentExample = Example.of(document, matcherCriteria);
        List<EngagementDocument> engagementDocumentList = repository.findAll(engagementDocumentExample);
        matchedEngagementList = sessionServiceHelper.engagementDocument2DetailedVo(engagementDocumentList);
        log.info("Found {} EngagementVo matching with provided parameters : {}", matchedEngagementList.size(), providedFilters);
        log.info("No EngagementVo available matching with provided parameters : {}", matchedEngagementList.size(), providedFilters);
        return matchedEngagementList;
    }

    @Override
    public String createEngagement(EngagementForm form) throws EngagementException {
        log.info("Creating new EngagementDocument");

        if(form == null) {
            log.debug("EngagementForm provided is null");
            throw new EngagementException(SessionErrorCode.SESSION_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of EngagementForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("EngagementForm has {} errors", err.getErrorCount());
            SessionErrorCode ec = SessionErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("EngagementForm error detail: {}", ec);
            throw new EngagementException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of EngagementForm are valid");

        EngagementDocument expectedDocument = form2DocumentConverter.convert(form);

        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_EXISTENCE_BY_ASSOCIATION_ID_EVENT_DATE_TIME.getValue(),
                form.getAssociationId(), form.getEvent(), form.getDate(), form.getTime());
        if(repository.existsByAssociationIdAndEventAndDateAndTime(
                expectedDocument.getAssociationId(), expectedDocument.getEvent(), expectedDocument.getDate(), expectedDocument.getTime())) {
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_EXISTS_BY_ASSOCIATION_ID_EVENT_DATE_TIME.getValue(),
                    expectedDocument.getAssociationId(), expectedDocument.getEvent(), expectedDocument.getDate(), expectedDocument.getTime());
            throw new EngagementException(SessionErrorCode.SESSION_EXISTS,
                    new Object[]{ "associationId: " + form.getAssociationId() + ", event: " + form.getEvent(), ", date: " + form.getDate() + ", time: " + form.getTime() });
        }
        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_NON_EXISTENCE_BY_ASSOCIATION_ID_EVENT_DATE_TIME.getValue(),
                expectedDocument.getAssociationId(), expectedDocument.getEvent(), expectedDocument.getDate(), expectedDocument.getTime());

        log.debug("Saving {}", expectedDocument);
        EngagementDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null) {
            log.debug("Unable to create {}", expectedDocument);
            throw new EngagementException(SessionErrorCode.SESSION_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist EngagementForm details" });
        }
        log.info("Created new EngagementForm with id: {}", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Override
    public void updateEngagement(String id, EngagementForm form) throws EngagementException {
        log.info("Updating EngagementForm by id: {}", id);

        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ENGAGEMENT_ENTITY_ID.getValue(), id);
        Optional<EngagementDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_NO_ENGAGEMENT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new EngagementException(SessionErrorCode.SESSION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_FOUND_ENGAGEMENT_ENTITY_ID.getValue(), id);

        EngagementDocument actualDocument = optActualDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("EngagementDocument is inactive with id: {}", id);
            throw new EngagementException(SessionErrorCode.SESSION_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("EngagementDocument is active with id: {}", id);

        if(form == null) {
            log.debug("EngagementForm is null");
            throw new EngagementException(SessionErrorCode.SESSION_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of EngagementForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("EngagementForm has {} errors", err.getErrorCount());
            SessionErrorCode ec = SessionErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("EngagementForm error detail: {}", ec);
            throw new EngagementException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of EngagementForm are empty");
            throw new EngagementException(SessionErrorCode.SESSION_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of EngagementForm are valid");

        Optional<EngagementDocument> optExpectedDocument = form2DocumentMapper.compareAndMap(actualDocument, form);
        if(optExpectedDocument.isEmpty()) {
            log.debug("No new value for attributes of EngagementForm");
            throw new EngagementException(SessionErrorCode.SESSION_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from EngagementForm to EngagementDocument");

        EngagementDocument expectedDocument = optExpectedDocument.get();

        this.checkUniquenessOfEngagement(form, actualDocument);

        documentSelfMapper.compareAndMap(expectedDocument, actualDocument);
        log.debug("Compared and copied attributes from EngagementDocument to EngagementForm");
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Updated: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to update {}", actualDocument);
            throw new EngagementException(SessionErrorCode.SESSION_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency engagement details" });
        }
        log.info("Updated existing EngagementDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void deleteEngagement(String id) throws EngagementException {
        log.info("Soft deleting EngagementDocument by id: {}", id);

        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ENGAGEMENT_ENTITY_ID.getValue(), id);
        Optional<EngagementDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_NO_ENGAGEMENT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new EngagementException(SessionErrorCode.SESSION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_FOUND_ENGAGEMENT_ENTITY_ID.getValue(), id);

        EngagementDocument actualDocument = optDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("EngagementDocument is inactive with id: {}", id);
            throw new EngagementException(SessionErrorCode.SESSION_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("EngagementDocument is active with id: {}", id);

        actualDocument.setActive(Boolean.FALSE);
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualDocument);
        EngagementDocument expectedDocument = repository.save(actualDocument);
        log.debug("Soft deleted: {}", expectedDocument);
        if(expectedDocument == null) {
            log.debug("Unable to soft delete {}", actualDocument);
            throw new EngagementException(SessionErrorCode.SESSION_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current engagement details with id:" + id });
        }

        log.info("Soft deleted existing EngagementDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void applyPatchOnEngagement(String id, List<PatchOperationForm> patches) throws EngagementException {
        log.info("Patching EngagementDocument by id: {}", id);

        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ENGAGEMENT_ENTITY_ID.getValue(), id);
        Optional<EngagementDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_NO_ENGAGEMENT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new EngagementException(SessionErrorCode.SESSION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_FOUND_ENGAGEMENT_ENTITY_ID.getValue(), id);

        EngagementDocument actualDocument = optActualDocument.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Engagement patch list not provided");
            throw new EngagementException(SessionErrorCode.SESSION_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Engagement patch list has {} items", patches.size());


        log.debug("Validating patch list items for Engagement");
        try {
            toabBaseService.validatePatches(patches, SessionErrorCode.SESSION_EXISTS.getDomain() + ":LOV");
            log.debug("All Engagement patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Engagement patch item are invalid");
            throw new EngagementException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Engagement");


        log.debug("Patching list items to EngagementDto");
        EngagementDto patchedEngagementForm = new EngagementDto();
        try {
            log.debug("Preparing patch list items for Engagement");
            JsonNode engagementDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch engagementPatch = JsonPatch.fromJson(engagementDtoTree);
            log.debug("Prepared patch list items for Engagement");
            JsonNode blankEngagementDtoTree = om.convertValue(new EngagementDto(), JsonNode.class);
            JsonNode patchedEngagementFormTree = engagementPatch.apply(blankEngagementDtoTree);
            log.debug("Applying patch list items to EngagementDto");
            patchedEngagementForm = om.treeToValue(patchedEngagementFormTree, EngagementDto.class);
            log.debug("Applied patch list items to EngagementDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to EngagementDto: {}", e);
            EngagementException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in EngagementDto");
                ex = new EngagementException(SessionErrorCode.SESSION_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new EngagementException(SessionErrorCode.SESSION_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to EngagementDto: {}", e);
            throw new EngagementException(SessionErrorCode.SESSION_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to EngagementDto");

        log.debug("Validating patched EngagementDto");
        Errors err = new DirectFieldBindingResult(patchedEngagementForm, patchedEngagementForm.getClass().getSimpleName());
        dtoValidator.validate(patchedEngagementForm, err);
        if(err.hasErrors()) {
            log.debug("Patched EngagementDto has {} errors", err.getErrorCount());
            SessionErrorCode ec = SessionErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched EngagementDto error detail: {}", ec);
            throw new EngagementException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched EngagementDto are valid");

        this.checkUniquenessOfEngagement(patchedEngagementForm, actualDocument);

        log.debug("Comparatively copying patched attributes from EngagementDto to EngagementDocument");
        try {
            dto2DocumentConverter.compareAndMap(patchedEngagementForm, actualDocument);
        } catch (TOABBaseException e) {
            throw (EngagementException) e;
        }
        log.debug("Comparatively copied patched attributes from EngagementDto to EngagementDocument");

        log.debug("Saving patched EngagementDocument: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Saved patched EngagementDocument: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to patch delete EngagementDocument with id:{}", id);
            throw new EngagementException(SessionErrorCode.SESSION_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency engagement details with id:" + id });
        }
        log.info("Patched EngagementDocument with id:{}", id);
    }

    private void checkUniquenessOfEngagement(EngagementDto patchedEngagementForm, EngagementDocument actualDocument) throws EngagementException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(2);
        if(patchedEngagementForm.getAssociationId().isPresent()) {
            similaritySwitchesCollection.add(patchedEngagementForm.getAssociationId().get().compareTo(actualDocument.getAssociationId()) == 0);
        }
        if(patchedEngagementForm.getEvent().isPresent()) {
            similaritySwitchesCollection.add(patchedEngagementForm.getEvent().get().toUpperCase().compareTo(actualDocument.getEvent().name()) == 0);
        }
        if(patchedEngagementForm.getDate().isPresent()) {
            LocalDate date = LocalDate.parse(patchedEngagementForm.getDate().get(), DateTimeFormatter.ofPattern(dateFormat));
            similaritySwitchesCollection.add(date.compareTo(actualDocument.getDate()) == 0);
        }
        if(patchedEngagementForm.getTime().isPresent()) {
            LocalTime time = LocalTime.parse(patchedEngagementForm.getTime().get(), DateTimeFormatter.ofPattern(timeFormat));
            similaritySwitchesCollection.add(time.compareTo(actualDocument.getTime()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String associationId = patchedEngagementForm.getAssociationId().isPresent() ? patchedEngagementForm.getAssociationId().get()
                    : actualDocument.getAssociationId();
            EngagementEvent event = patchedEngagementForm.getEvent().isPresent() ? EngagementEvent.valueOf(patchedEngagementForm.getEvent().get())
                    : actualDocument.getEvent();
            LocalDate date = patchedEngagementForm.getDate().isPresent() ? LocalDate.parse(patchedEngagementForm.getDate().get(), DateTimeFormatter.ofPattern(dateFormat))
                    : actualDocument.getDate();
            LocalTime time = patchedEngagementForm.getTime().isPresent() ? LocalTime.parse(patchedEngagementForm.getTime().get(), DateTimeFormatter.ofPattern(timeFormat))
                    : actualDocument.getTime();
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_EXISTENCE_BY_ASSOCIATION_ID_EVENT_DATE_TIME.getValue(), associationId, event, date, time);
            boolean sameDocumentSw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateDocumentSw =  repository.existsByAssociationIdAndEventAndDateAndTime(associationId, event, date, time);
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_EXISTS_BY_ASSOCIATION_ID_EVENT_DATE_TIME.getValue(), associationId, event, date, time);
                throw new EngagementException(SessionErrorCode.SESSION_EXISTS,
                        new Object[]{ "associationId: " + associationId + ", event: " + event, ", date: " + date + ", time: " + time });
            }
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_NON_EXISTENCE_BY_ASSOCIATION_ID_EVENT_DATE_TIME.getValue(), associationId, event, date, time);

        }
    }

    private void checkUniquenessOfEngagement(EngagementForm engagementForm, EngagementDocument actualDocument) throws EngagementException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(3);
        if(StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getAssociationId()))) {
            similaritySwitchesCollection.add(engagementForm.getAssociationId().compareTo(actualDocument.getAssociationId()) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getEvent()))) {
            similaritySwitchesCollection.add(engagementForm.getEvent().toUpperCase().compareTo(actualDocument.getEvent().name()) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getDate()))) {
            LocalDate date = LocalDate.parse(engagementForm.getDate(), DateTimeFormatter.ofPattern(dateFormat));
            similaritySwitchesCollection.add(date.compareTo(actualDocument.getDate()) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getTime()))) {
            LocalTime time = LocalTime.parse(engagementForm.getTime(), DateTimeFormatter.ofPattern(timeFormat));
            similaritySwitchesCollection.add(time.compareTo(actualDocument.getTime()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String associationId = StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getAssociationId())) ? engagementForm.getAssociationId() : actualDocument.getAssociationId();
            EngagementEvent event = StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getEvent())) ? EngagementEvent.valueOf(engagementForm.getEvent())
                    : actualDocument.getEvent();
            LocalDate date = StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getDate())) ? LocalDate.parse(engagementForm.getDate(), DateTimeFormatter.ofPattern(dateFormat))
                    : actualDocument.getDate();
            LocalTime time = StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getTime())) ? LocalTime.parse(engagementForm.getTime(), DateTimeFormatter.ofPattern(timeFormat))
                    : actualDocument.getTime();

            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_EXISTENCE_BY_ASSOCIATION_ID_EVENT_DATE_TIME.getValue(), associationId, event, date, time);
            boolean sameDocumentSw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateDocumentSw =  repository.existsByAssociationIdAndEventAndDateAndTime(associationId, event, date, time);
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_EXISTS_BY_ASSOCIATION_ID_EVENT_DATE_TIME.getValue(), associationId, event, date, time);
                throw new EngagementException(SessionErrorCode.SESSION_EXISTS,
                        new Object[]{ "associationId: " + associationId + ", event: " + event, ", date: " + date + ", time: " + time });
            }
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_NON_EXISTENCE_BY_ASSOCIATION_ID_EVENT_DATE_TIME.getValue(), associationId, event, date, time);

        }
    }
    
}