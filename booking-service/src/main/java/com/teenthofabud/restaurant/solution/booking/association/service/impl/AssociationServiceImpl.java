package com.teenthofabud.restaurant.solution.booking.association.service.impl;

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
import com.teenthofabud.restaurant.solution.booking.association.converter.AssociationDto2DocumentConverter;
import com.teenthofabud.restaurant.solution.booking.association.converter.AssociationForm2DocumentConverter;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationDocument;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationDto;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationException;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationForm;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationMessageTemplate;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationVo;
import com.teenthofabud.restaurant.solution.booking.association.mapper.AssociationDocumentSelfMapper;
import com.teenthofabud.restaurant.solution.booking.association.mapper.AssociationForm2DocumentMapper;
import com.teenthofabud.restaurant.solution.booking.association.repository.AssociationRepository;
import com.teenthofabud.restaurant.solution.booking.association.service.AssociationService;
import com.teenthofabud.restaurant.solution.booking.association.validator.AssociationDtoValidator;
import com.teenthofabud.restaurant.solution.booking.association.validator.AssociationFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.booking.association.validator.AssociationFormValidator;
import com.teenthofabud.restaurant.solution.booking.error.BookingErrorCode;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceException;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceVo;
import com.teenthofabud.restaurant.solution.booking.experience.service.ExperienceService;
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
public class AssociationServiceImpl implements AssociationService {

    private static final Comparator<AssociationVo> CMP_BY_EXPERIENCE_TABLE_ACCOUNT = (s1, s2) -> {
       int c1 = s1.getExperienceId().compareTo(s2.getExperienceId());
       if(c1 == 0) {
           int c2 = s1.getTableId().compareTo(s2.getTableId());
           if(c2 == 0) {
               int c3 = s1.getAccountId().compareTo(s2.getAccountId());
               return c3;
           } else {
               return c2;
           }
       } else {
           return c1;
       }
    };

    private AssociationForm2DocumentConverter form2DocumentConverter;
    private AssociationDto2DocumentConverter dto2DocumentConverter;
    private AssociationForm2DocumentMapper form2DocumentMapper;
    private AssociationDocumentSelfMapper documentSelfMapper;
    private AssociationFormValidator formValidator;
    private AssociationFormRelaxedValidator relaxedFormValidator;
    private AssociationDtoValidator dtoValidator;
    private AssociationRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private BookingServiceHelper bookingServiceHelper;
    private ExperienceService experienceService;

    @Autowired
    public void setExperienceService(ExperienceService experienceService) {
        this.experienceService = experienceService;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }


    @Autowired
    public void setSessionServiceHelper(BookingServiceHelper bookingServiceHelper) {
        this.bookingServiceHelper = bookingServiceHelper;
    }

    @Autowired
    public void setDto2DocumentConverter(AssociationDto2DocumentConverter dto2DocumentConverter) {
        this.dto2DocumentConverter = dto2DocumentConverter;
    }

    @Autowired
    public void setForm2DocumentMapper(AssociationForm2DocumentMapper form2DocumentMapper) {
        this.form2DocumentMapper = form2DocumentMapper;
    }

    @Autowired
    public void setDocumentSelfMapper(AssociationDocumentSelfMapper documentSelfMapper) {
        this.documentSelfMapper = documentSelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(AssociationFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchAssociationValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(AssociationDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2DocumentConverter(AssociationForm2DocumentConverter form2DocumentConverter) {
        this.form2DocumentConverter = form2DocumentConverter;
    }

    @Autowired
    public void setRepository(AssociationRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(AssociationFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseAssociationId(String id) throws AssociationException {
        Long associationId = null;
        try {
            associationId = Long.parseLong(id);
            log.debug("Parsed id {} to association id {} in numeric format", id, associationId);
            if(associationId <= 0) {
                throw new NumberFormatException("association id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse association id", e);
            log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_ID_INVALID.getValue(), id);
            throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return associationId;
    }

    @Override
    public Set<AssociationVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all AssociationDocument by their natural ordering");
        List<AssociationDocument> associationDocumentList = repository.findAll();
        List<AssociationVo> associationVoList = bookingServiceHelper.associationDocument2DetailedVo(associationDocumentList);
        Set<AssociationVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_EXPERIENCE_TABLE_ACCOUNT);
        naturallyOrderedSet.addAll(associationVoList);
        log.info("{} AssociationVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public AssociationVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws AssociationException {
        log.info("Requesting AssociationDocument by id: {}", id);
        Optional<AssociationDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug("No AssociationDocument found by id: {}", id);
            throw new AssociationException(BookingErrorCode.BOOKING_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found AssociationVo by id: {}", id);
        AssociationDocument document = optDocument.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        AssociationVo vo = bookingServiceHelper.associationDocument2DetailedVo(document);
        log.debug("AssociationVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<AssociationVo> retrieveAllMatchingDetailsByExperienceId(String experienceId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws AssociationException {
        log.info("Requesting AssociationDocument that match with experienceId: {}", experienceId);
        Errors err = new DirectFieldBindingResult(experienceId, "AssociationForm");
        try {
            ExperienceVo experienceVo = experienceService.retrieveDetailsById(experienceId, Optional.of(TOABCascadeLevel.ONE));
            if(!experienceVo.getActive()) {
                throw new ExperienceException(BookingErrorCode.BOOKING_INACTIVE, new Object [] { experienceId });
            }
        } catch (ExperienceException e) {
            log.error("experienceId is invalid", e);
            throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object [] { "experienceId: " + experienceId });
        }
        List<AssociationDocument> associationDocumentList = repository.findByExperienceId(experienceId);
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        List<AssociationVo> matchedAssociationList = bookingServiceHelper.associationDocument2DetailedVo(associationDocumentList);
        log.info("Found {} AssociationVo matching with recipeId: {}", matchedAssociationList.size(), experienceId);
        TOABRequestContextHolder.clearCascadeLevelContext();
        if(matchedAssociationList.isEmpty()) {
            log.debug("No AssociationVo found matching with experienceId: {}", experienceId);
            throw new AssociationException(BookingErrorCode.BOOKING_NOT_FOUND, new Object[] { "experienceId", experienceId });
        }
        return matchedAssociationList;
    }

    @Override
    public List<AssociationVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalTableId, Optional<String> optionalAccountId) throws AssociationException {
        if(optionalTableId.isEmpty() && optionalAccountId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String tableId = optionalTableId.isPresent() ? optionalTableId.get() : "";
        String accountId = optionalAccountId.isPresent() ? optionalAccountId.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(tableId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(accountId))) {
            log.debug("All search parameters are empty");
        }
        List<AssociationVo> matchedAssociationList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        AssociationDocument document = new AssociationDocument();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(tableId))) {
            log.debug("tableId {} is valid", tableId);
            providedFilters.put("tableId", tableId);
            document.setTableId(tableId);
            matcherCriteria = matcherCriteria.withMatcher("tableId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(accountId))) {
            log.debug("accountId {} is valid", accountId);
            providedFilters.put("accountId", accountId);
            document.setAccountId(accountId);
            matcherCriteria = matcherCriteria.withMatcher("accountId", match -> match.exact());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<AssociationDocument> associationDocumentExample = Example.of(document, matcherCriteria);
        List<AssociationDocument> associationDocumentList = repository.findAll(associationDocumentExample);
        matchedAssociationList = bookingServiceHelper.associationDocument2DetailedVo(associationDocumentList);
        log.info("Found {} AssociationVo matching with provided parameters : {}", matchedAssociationList.size(), providedFilters);
        log.info("No AssociationVo available matching with provided parameters : {}", matchedAssociationList.size(), providedFilters);
        return matchedAssociationList;
    }

    @Override
    public String createAssociation(AssociationForm form) throws AssociationException {
        log.info("Creating new AssociationDocument");

        if(form == null) {
            log.debug("AssociationForm provided is null");
            throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of AssociationForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("AssociationForm has {} errors", err.getErrorCount());
            BookingErrorCode ec = BookingErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("AssociationForm error detail: {}", ec);
            throw new AssociationException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of AssociationForm are valid");

        AssociationDocument expectedDocument = form2DocumentConverter.convert(form);

        log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_EXISTENCE_BY_TABLE_ID_AND_ACCOUNT_ID.getValue(), form.getTableId(), form.getAccountId());
        if(repository.existsByTableIdAndAccountId(expectedDocument.getTableId(), expectedDocument.getAccountId())) {
            log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_EXISTS_BY_TABLE_ID_AND_ACCOUNT_ID.getValue(), expectedDocument.getTableId(), expectedDocument.getAccountId());
            throw new AssociationException(BookingErrorCode.BOOKING_EXISTS,
                    new Object[]{ "tableId: " + form.getTableId(), ", accountId: " + form.getAccountId() });
        }
        log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_NON_EXISTENCE_BY_TABLE_ID_AND_ACCOUNT_ID.getValue(), expectedDocument.getTableId(), expectedDocument.getAccountId());

        log.debug("Saving {}", expectedDocument);
        AssociationDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null) {
            log.debug("Unable to create {}", expectedDocument);
            throw new AssociationException(BookingErrorCode.BOOKING_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist AssociationForm details" });
        }
        log.info("Created new AssociationForm with id: {}", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Override
    public void updateAssociation(String id, AssociationForm form) throws AssociationException {
        log.info("Updating AssociationForm by id: {}", id);

        log.debug(AssociationMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ASSOCIATION_ENTITY_ID.getValue(), id);
        Optional<AssociationDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(AssociationMessageTemplate.MSG_TEMPLATE_NO_ASSOCIATION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new AssociationException(BookingErrorCode.BOOKING_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(AssociationMessageTemplate.MSG_TEMPLATE_FOUND_ASSOCIATION_ENTITY_ID.getValue(), id);

        AssociationDocument actualDocument = optActualDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("AssociationDocument is inactive with id: {}", id);
            throw new AssociationException(BookingErrorCode.BOOKING_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("AssociationDocument is active with id: {}", id);

        if(form == null) {
            log.debug("AssociationForm is null");
            throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of AssociationForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("AssociationForm has {} errors", err.getErrorCount());
            BookingErrorCode ec = BookingErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("AssociationForm error detail: {}", ec);
            throw new AssociationException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of AssociationForm are empty");
            throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of AssociationForm are valid");

        Optional<AssociationDocument> optExpectedDocument = form2DocumentMapper.compareAndMap(actualDocument, form);
        if(optExpectedDocument.isEmpty()) {
            log.debug("No new value for attributes of AssociationForm");
            throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from AssociationForm to AssociationDocument");

        AssociationDocument expectedDocument = optExpectedDocument.get();

        this.checkUniquenessOfAssociation(form, actualDocument);

        documentSelfMapper.compareAndMap(expectedDocument, actualDocument);
        log.debug("Compared and copied attributes from AssociationDocument to AssociationForm");
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Updated: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to update {}", actualDocument);
            throw new AssociationException(BookingErrorCode.BOOKING_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency association details" });
        }
        log.info("Updated existing AssociationDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void deleteAssociation(String id) throws AssociationException {
        log.info("Soft deleting AssociationDocument by id: {}", id);

        log.debug(AssociationMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ASSOCIATION_ENTITY_ID.getValue(), id);
        Optional<AssociationDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug(AssociationMessageTemplate.MSG_TEMPLATE_NO_ASSOCIATION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new AssociationException(BookingErrorCode.BOOKING_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(AssociationMessageTemplate.MSG_TEMPLATE_FOUND_ASSOCIATION_ENTITY_ID.getValue(), id);

        AssociationDocument actualDocument = optDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("AssociationDocument is inactive with id: {}", id);
            throw new AssociationException(BookingErrorCode.BOOKING_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("AssociationDocument is active with id: {}", id);

        actualDocument.setActive(Boolean.FALSE);
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualDocument);
        AssociationDocument expectedDocument = repository.save(actualDocument);
        log.debug("Soft deleted: {}", expectedDocument);
        if(expectedDocument == null) {
            log.debug("Unable to soft delete {}", actualDocument);
            throw new AssociationException(BookingErrorCode.BOOKING_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current association details with id:" + id });
        }

        log.info("Soft deleted existing AssociationDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void applyPatchOnAssociation(String id, List<PatchOperationForm> patches) throws AssociationException {
        log.info("Patching AssociationDocument by id: {}", id);

        log.debug(AssociationMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ASSOCIATION_ENTITY_ID.getValue(), id);
        Optional<AssociationDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(AssociationMessageTemplate.MSG_TEMPLATE_NO_ASSOCIATION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new AssociationException(BookingErrorCode.BOOKING_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(AssociationMessageTemplate.MSG_TEMPLATE_FOUND_ASSOCIATION_ENTITY_ID.getValue(), id);

        AssociationDocument actualDocument = optActualDocument.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Association patch list not provided");
            throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Association patch list has {} items", patches.size());


        log.debug("Validating patch list items for Association");
        try {
            toabBaseService.validatePatches(patches, BookingErrorCode.BOOKING_EXISTS.getDomain() + ":LOV");
            log.debug("All Association patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Association patch item are invalid");
            throw new AssociationException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Association");


        log.debug("Patching list items to AssociationDto");
        AssociationDto patchedAssociationForm = new AssociationDto();
        try {
            log.debug("Preparing patch list items for Association");
            JsonNode associationDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch associationPatch = JsonPatch.fromJson(associationDtoTree);
            log.debug("Prepared patch list items for Association");
            JsonNode blankAssociationDtoTree = om.convertValue(new AssociationDto(), JsonNode.class);
            JsonNode patchedAssociationFormTree = associationPatch.apply(blankAssociationDtoTree);
            log.debug("Applying patch list items to AssociationDto");
            patchedAssociationForm = om.treeToValue(patchedAssociationFormTree, AssociationDto.class);
            log.debug("Applied patch list items to AssociationDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to AssociationDto: {}", e);
            AssociationException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in AssociationDto");
                ex = new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new AssociationException(BookingErrorCode.BOOKING_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to AssociationDto: {}", e);
            throw new AssociationException(BookingErrorCode.BOOKING_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to AssociationDto");

        log.debug("Validating patched AssociationDto");
        Errors err = new DirectFieldBindingResult(patchedAssociationForm, patchedAssociationForm.getClass().getSimpleName());
        dtoValidator.validate(patchedAssociationForm, err);
        if(err.hasErrors()) {
            log.debug("Patched AssociationDto has {} errors", err.getErrorCount());
            BookingErrorCode ec = BookingErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched AssociationDto error detail: {}", ec);
            throw new AssociationException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched AssociationDto are valid");

        this.checkUniquenessOfAssociation(patchedAssociationForm, actualDocument);

        log.debug("Comparatively copying patched attributes from AssociationDto to AssociationDocument");
        try {
            dto2DocumentConverter.compareAndMap(patchedAssociationForm, actualDocument);
        } catch (TOABBaseException e) {
            throw (AssociationException) e;
        }
        log.debug("Comparatively copied patched attributes from AssociationDto to AssociationDocument");

        log.debug("Saving patched AssociationDocument: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Saved patched AssociationDocument: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to patch delete AssociationDocument with id:{}", id);
            throw new AssociationException(BookingErrorCode.BOOKING_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency association details with id:" + id });
        }
        log.info("Patched AssociationDocument with id:{}", id);
    }

    private void checkUniquenessOfAssociation(AssociationDto patchedAssociationForm, AssociationDocument actualDocument) throws AssociationException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(2);
        if(patchedAssociationForm.getTableId().isPresent()) {
            similaritySwitchesCollection.add(patchedAssociationForm.getTableId().get().compareTo(actualDocument.getTableId()) == 0);
        }
        if(patchedAssociationForm.getAccountId().isPresent()) {
            similaritySwitchesCollection.add(patchedAssociationForm.getAccountId().get().compareTo(actualDocument.getAccountId()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String tableId = patchedAssociationForm.getTableId().isPresent() ? patchedAssociationForm.getTableId().get() : actualDocument.getTableId();
            String accountId = patchedAssociationForm.getAccountId().isPresent() ? patchedAssociationForm.getAccountId().get() : actualDocument.getAccountId();
            log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_EXISTENCE_BY_TABLE_ID_AND_ACCOUNT_ID.getValue(), tableId, accountId);
            boolean sameDocumentSw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateDocumentSw =  repository.existsByTableIdAndAccountId(tableId, accountId);
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_EXISTS_BY_TABLE_ID_AND_ACCOUNT_ID.getValue(), tableId, accountId);
                throw new AssociationException(BookingErrorCode.BOOKING_EXISTS, new Object[]{ "tableId: " + tableId, ", accountId: " + accountId });
            }
            log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_NON_EXISTENCE_BY_TABLE_ID_AND_ACCOUNT_ID.getValue(), tableId, accountId);

        }
    }

    private void checkUniquenessOfAssociation(AssociationForm associationForm, AssociationDocument actualDocument) throws AssociationException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(3);
        if(StringUtils.hasText(StringUtils.trimWhitespace(associationForm.getTableId()))) {
            similaritySwitchesCollection.add(associationForm.getTableId().compareTo(actualDocument.getTableId()) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(associationForm.getAccountId()))) {
            similaritySwitchesCollection.add(associationForm.getAccountId().compareTo(actualDocument.getAccountId()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String tableId = StringUtils.hasText(StringUtils.trimWhitespace(associationForm.getTableId())) ? associationForm.getTableId() : actualDocument.getTableId();
            String accountId = StringUtils.hasText(StringUtils.trimWhitespace(associationForm.getAccountId())) ? associationForm.getAccountId() : actualDocument.getAccountId();
            log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_EXISTENCE_BY_TABLE_ID_AND_ACCOUNT_ID.getValue(), tableId, accountId);
            boolean sameDocumentSw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateDocumentSw =  repository.existsByTableIdAndAccountId(tableId, accountId);
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_EXISTS_BY_TABLE_ID_AND_ACCOUNT_ID.getValue(), tableId, accountId);
                throw new AssociationException(BookingErrorCode.BOOKING_EXISTS, new Object[]{ "tableId: " + tableId, ", accountId: " + accountId });
            }
            log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_NON_EXISTENCE_BY_TABLE_ID_AND_ACCOUNT_ID.getValue(), tableId, accountId);

        }
    }
    
}