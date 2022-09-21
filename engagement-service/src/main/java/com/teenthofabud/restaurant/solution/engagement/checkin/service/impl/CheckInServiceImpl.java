package com.teenthofabud.restaurant.solution.engagement.checkin.service.impl;

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
import com.teenthofabud.restaurant.solution.engagement.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.CheckInDto2DocumentConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.CheckInForm2DocumentConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInDocument;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInDto;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInMessageTemplate;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.CheckInEntitySelfMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.CheckInForm2EntityMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.CheckInRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.CheckInService;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.CheckInDtoValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.CheckInFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.CheckInFormValidator;
import com.teenthofabud.restaurant.solution.engagement.error.EngagementErrorCode;
import com.teenthofabud.restaurant.solution.engagement.integration.customer.validator.AccountIdValidator;
import com.teenthofabud.restaurant.solution.engagement.utils.EngagementServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;
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
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

@Component
@Slf4j
public class CheckInServiceImpl implements CheckInService {

    private static final Comparator<CheckInVo> CMP_BY_TABLE_TOKEN_ACCOUNT = (s1, s2) -> {
       int c1 = s1.getTableId().compareTo(s2.getTableId());
       if(c1 == 0) {
           int c2 = s1.getSequence().compareTo(s2.getSequence());
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

    private CheckInForm2DocumentConverter form2DocumentConverter;
    private CheckInDto2DocumentConverter dto2DocumentConverter;
    private CheckInForm2EntityMapper form2DocumentMapper;
    private CheckInEntitySelfMapper documentSelfMapper;
    private CheckInFormValidator formValidator;
    private CheckInFormRelaxedValidator relaxedFormValidator;
    private CheckInDtoValidator dtoValidator;
    private CheckInRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private EngagementServiceHelper engagementServiceHelper;
    private CategoryService categoryService;
    private AccountIdValidator accountIdValidator;
    private String checkInTimeFormat;

    @Value("${res.engagement.checkIn.timestamp}")
    public void setCheckInTimeFormat(String checkInTimeFormat) {
        this.checkInTimeFormat = checkInTimeFormat;
    }

    @Autowired
    public void setAccountIdValidator(AccountIdValidator accountIdValidator) {
        this.accountIdValidator = accountIdValidator;
    }

    @Autowired
    public void setEngagementServiceHelper(EngagementServiceHelper engagementServiceHelper) {
        this.engagementServiceHelper = engagementServiceHelper;
    }

    @Autowired
    public void setCategoryService(CategoryService categoryService) {
        this.categoryService = categoryService;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setDto2DocumentConverter(CheckInDto2DocumentConverter dto2DocumentConverter) {
        this.dto2DocumentConverter = dto2DocumentConverter;
    }

    @Autowired
    public void setForm2DocumentMapper(CheckInForm2EntityMapper form2DocumentMapper) {
        this.form2DocumentMapper = form2DocumentMapper;
    }

    @Autowired
    public void setDocumentSelfMapper(CheckInEntitySelfMapper documentSelfMapper) {
        this.documentSelfMapper = documentSelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(CheckInFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchCheckInValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(CheckInDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2DocumentConverter(CheckInForm2DocumentConverter form2DocumentConverter) {
        this.form2DocumentConverter = form2DocumentConverter;
    }

    @Autowired
    public void setRepository(CheckInRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(CheckInFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseCheckInId(String id) throws CheckInException {
        Long checkInId = null;
        try {
            checkInId = Long.parseLong(id);
            log.debug("Parsed id {} to checkIn id {} in numeric format", id, checkInId);
            if(checkInId <= 0) {
                throw new NumberFormatException("checkIn id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse checkIn id", e);
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_ID_INVALID.getValue(), id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return checkInId;
    }

    @Override
    public Set<CheckInVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all CheckInDocument by their natural ordering");
        List<CheckInDocument> checkInDocumentList = repository.findAll();
        List<CheckInVo> checkInVoList = engagementServiceHelper.checkInDocument2DetailedVo(checkInDocumentList);
        Set<CheckInVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_TABLE_TOKEN_ACCOUNT);
        naturallyOrderedSet.addAll(checkInVoList);
        log.info("{} CheckInVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public CheckInVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws CheckInException {
        log.info("Requesting CheckInDocument by id: {}", id);
        Optional<CheckInDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug("No CheckInDocument found by id: {}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found CheckInVo by id: {}", id);
        CheckInDocument document = optDocument.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        CheckInVo vo = engagementServiceHelper.checkInDocument2DetailedVo(document);
        log.debug("CheckInVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<CheckInVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalAccountId, Optional<String> optionalTableId, Optional<String> optionalName, Optional<String> optionalPhoneNumber, Optional<String> optionalEmailId, Optional<String> optionalNotes) throws CheckInException {
        if(optionalAccountId.isEmpty() && optionalTableId.isEmpty() && optionalPhoneNumber.isEmpty() && optionalName.isEmpty() && optionalEmailId.isEmpty() && optionalNotes.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String accountId = optionalAccountId.isPresent() ? optionalAccountId.get() : "";
        String tableId = optionalTableId.isPresent() ? optionalTableId.get() : "";
        String phoneNumber = optionalPhoneNumber.isPresent() ? optionalPhoneNumber.get() : "";
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String emailId = optionalEmailId.isPresent() ? optionalEmailId.get() : "";
        String notes = optionalNotes.isPresent() ? optionalNotes.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(accountId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(tableId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(phoneNumber))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(emailId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(notes))) {
            log.debug("All search parameters are empty");
        }
        List<CheckInVo> matchedCheckInList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        CheckInDocument document = new CheckInDocument();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(accountId))) {
            log.debug("accountId {} is valid", accountId);
            providedFilters.put("accountId", accountId);
            document.setAccountId(accountId);
            matcherCriteria = matcherCriteria.withMatcher("accountId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(tableId))) {
            log.debug("tableId {} is valid", tableId);
            providedFilters.put("tableId", tableId);
            document.setTableId(tableId);
            matcherCriteria = matcherCriteria.withMatcher("tableId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            log.debug("name {} is valid", accountId);
            providedFilters.put("name", accountId);
            document.setName(name);
            matcherCriteria = matcherCriteria.withMatcher("name", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(phoneNumber))) {
            log.debug("phoneNumber {} is valid", accountId);
            providedFilters.put("phoneNumber", accountId);
            document.setPhoneNumber(phoneNumber);
            matcherCriteria = matcherCriteria.withMatcher("phoneNumber", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(emailId))) {
            log.debug("emailId {} is valid", accountId);
            providedFilters.put("emailId", accountId);
            document.setPhoneNumber(emailId);
            matcherCriteria = matcherCriteria.withMatcher("emailId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(notes))) {
            log.debug("notes {} is valid", accountId);
            providedFilters.put("notes", accountId);
            document.setNotes(notes);
            matcherCriteria = matcherCriteria.withMatcher("notes", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<CheckInDocument> checkInDocumentExample = Example.of(document, matcherCriteria);
        List<CheckInDocument> checkInDocumentList = repository.findAll(checkInDocumentExample);
        matchedCheckInList = engagementServiceHelper.checkInDocument2DetailedVo(checkInDocumentList);
        log.info("Found {} CheckInVo matching with provided parameters : {}", matchedCheckInList.size(), providedFilters);
        log.info("No CheckInVo available matching with provided parameters : {}", matchedCheckInList.size(), providedFilters);
        return matchedCheckInList;
    }

    @Override
    public CheckInVo retrieveAllMatchingDetailsByCriteria(String sequence, String date) throws CheckInException {
        Long seq = 0l;
        LocalDate dt = LocalDate.now();
        LocalDateTime start = LocalDateTime.now();
        LocalDateTime end = LocalDateTime.now();
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(checkInTimeFormat);

        try {
            seq = Long.parseLong(sequence);
        } catch (NumberFormatException e) {
            log.debug("Sequence: {} format is invalid", sequence);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "sequece", sequence });
        }

        try {
            dt = LocalDate.parse(date, dtf);
        } catch (DateTimeParseException e) {
            log.debug("Date: {} format is invalid", sequence);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "sequece", sequence });
        }

        start = LocalDateTime.of(dt, LocalTime.of(0,0, 0));
        end = LocalDateTime.of(dt, LocalTime.of(23,59, 59));

        log.info("Requesting CheckInDocument by sequence: {} between timestamps: {} and {}", seq, start, end);
        Optional<CheckInDocument> optDocument = repository.findBySequenceAndCreatedOnBetween(seq, start, end);
        if(optDocument.isEmpty()) {
            log.debug("No CheckInDocument found by sequence: {} between timestamps: {} and {}", seq, start, end);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "seq: " + seq, ", date: " + date });
        }
        log.info("Found CheckInVo by sequence: {} between timestamps: {} and {}", seq, start, end);
        CheckInDocument document = optDocument.get();
        CheckInVo vo = engagementServiceHelper.checkInDocument2DetailedVo(document);
        return vo;
    }

    @Override
    public String createCheckIn(CheckInForm form) throws CheckInException {
        log.info("Creating new CheckInDocument");

        if(form == null) {
            log.debug("CheckInForm provided is null");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of CheckInForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("CheckInForm has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("CheckInForm error detail: {}", ec);
            throw new CheckInException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of CheckInForm are valid");

        CheckInDocument expectedDocument = form2DocumentConverter.convert(form);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTENCE_BY_TABLE_ID_AND_ACCOUNT_ID_AND_SEQUENCE.getValue(), form.getTableId(), form.getAccountId(), form.getSequence());
        if(repository.existsByAccountIdAndTableIdAndSequence(expectedDocument.getAccountId(), expectedDocument.getTableId(), expectedDocument.getSequence())) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTS_BY_TABLE_ID_AND_ACCOUNT_ID_AND_SEQUENCE.getValue(),
                    expectedDocument.getTableId(), expectedDocument.getAccountId(), expectedDocument.getSequence());
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_EXISTS,
                    new Object[]{ "tableId: " + form.getTableId(), "accountId: " + form.getAccountId() + ", sequence: " + form.getSequence() });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_NON_EXISTENCE_BY_TABLE_ID_AND_ACCOUNT_ID_AND_SEQUENCE.getValue(), expectedDocument.getTableId(), expectedDocument.getAccountId(),
                expectedDocument.getSequence());

        log.debug("Saving {}", expectedDocument);
        CheckInDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null) {
            log.debug("Unable to create {}", expectedDocument);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist CheckInForm details" });
        }
        log.info("Created new CheckInForm with id: {}", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Override
    public void updateCheckIn(String id, CheckInForm form) throws CheckInException {
        log.info("Updating CheckInForm by id: {}", id);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Optional<CheckInDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        CheckInDocument actualDocument = optActualDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("CheckInDocument is inactive with id: {}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("CheckInDocument is active with id: {}", id);

        if(form == null) {
            log.debug("CheckInForm is null");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of CheckInForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("CheckInForm has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("CheckInForm error detail: {}", ec);
            throw new CheckInException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of CheckInForm are empty");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of CheckInForm are valid");

        Optional<CheckInDocument> optExpectedDocument = form2DocumentMapper.compareAndMap(actualDocument, form);
        if(optExpectedDocument.isEmpty()) {
            log.debug("No new value for attributes of CheckInForm");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from CheckInForm to CheckInDocument");

        CheckInDocument expectedDocument = optExpectedDocument.get();

        this.checkUniquenessOfCheckIn(form, actualDocument);

        documentSelfMapper.compareAndMap(expectedDocument, actualDocument);
        log.debug("Compared and copied attributes from CheckInDocument to CheckInForm");
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Updated: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to update {}", actualDocument);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency checkIn details" });
        }
        log.info("Updated existing CheckInDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void deleteCheckIn(String id) throws CheckInException {
        log.info("Soft deleting CheckInDocument by id: {}", id);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Optional<CheckInDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        CheckInDocument actualDocument = optDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("CheckInDocument is inactive with id: {}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("CheckInDocument is active with id: {}", id);

        actualDocument.setActive(Boolean.FALSE);
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualDocument);
        CheckInDocument expectedDocument = repository.save(actualDocument);
        log.debug("Soft deleted: {}", expectedDocument);
        if(expectedDocument == null) {
            log.debug("Unable to soft delete {}", actualDocument);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current checkIn details with id:" + id });
        }

        log.info("Soft deleted existing CheckInDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void applyPatchOnCheckIn(String id, List<PatchOperationForm> patches) throws CheckInException {
        log.info("Patching CheckInDocument by id: {}", id);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Optional<CheckInDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        CheckInDocument actualDocument = optActualDocument.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("CheckIn patch list not provided");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("CheckIn patch list has {} items", patches.size());


        log.debug("Validating patch list items for CheckIn");
        try {
            toabBaseService.validatePatches(patches, EngagementErrorCode.ENGAGEMENT_EXISTS.getDomain() + ":LOV");
            log.debug("All CheckIn patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the CheckIn patch item are invalid");
            throw new CheckInException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for CheckIn");


        log.debug("Patching list items to CheckInDto");
        CheckInDto patchedCheckInForm = new CheckInDto();
        try {
            log.debug("Preparing patch list items for CheckIn");
            JsonNode checkInDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch checkInPatch = JsonPatch.fromJson(checkInDtoTree);
            log.debug("Prepared patch list items for CheckIn");
            JsonNode blankCheckInDtoTree = om.convertValue(new CheckInDto(), JsonNode.class);
            JsonNode patchedCheckInFormTree = checkInPatch.apply(blankCheckInDtoTree);
            log.debug("Applying patch list items to CheckInDto");
            patchedCheckInForm = om.treeToValue(patchedCheckInFormTree, CheckInDto.class);
            log.debug("Applied patch list items to CheckInDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to CheckInDto: {}", e);
            CheckInException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in CheckInDto");
                ex = new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to CheckInDto: {}", e);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to CheckInDto");

        log.debug("Validating patched CheckInDto");
        Errors err = new DirectFieldBindingResult(patchedCheckInForm, patchedCheckInForm.getClass().getSimpleName());
        dtoValidator.validate(patchedCheckInForm, err);
        if(err.hasErrors()) {
            log.debug("Patched CheckInDto has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched CheckInDto error detail: {}", ec);
            throw new CheckInException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched CheckInDto are valid");

        this.checkUniquenessOfCheckIn(patchedCheckInForm, actualDocument);

        log.debug("Comparatively copying patched attributes from CheckInDto to CheckInDocument");
        try {
            dto2DocumentConverter.compareAndMap(patchedCheckInForm, actualDocument);
        } catch (TOABBaseException e) {
            throw (CheckInException) e;
        }
        log.debug("Comparatively copied patched attributes from CheckInDto to CheckInDocument");

        log.debug("Saving patched CheckInDocument: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Saved patched CheckInDocument: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to patch delete CheckInDocument with id:{}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency checkIn details with id:" + id });
        }
        log.info("Patched CheckInDocument with id:{}", id);
    }

    private void checkUniquenessOfCheckIn(CheckInDto patchedCheckInForm, CheckInDocument actualDocument) throws CheckInException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(2);
        if(patchedCheckInForm.getAccountId().isPresent()) {
            similaritySwitchesCollection.add(patchedCheckInForm.getAccountId().get().compareTo(actualDocument.getAccountId()) == 0);
        }
        if(patchedCheckInForm.getSequence().isPresent()) {
            similaritySwitchesCollection.add(patchedCheckInForm.getSequence().get().compareTo(actualDocument.getSequence()) == 0);
        }
        if(patchedCheckInForm.getTableId().isPresent()) {
            similaritySwitchesCollection.add(patchedCheckInForm.getTableId().get().compareTo(actualDocument.getTableId()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String accountId = patchedCheckInForm.getAccountId().isPresent() ? patchedCheckInForm.getAccountId().get() : actualDocument.getAccountId();
            String tableId = patchedCheckInForm.getTableId().isPresent() ? patchedCheckInForm.getTableId().get() : actualDocument.getTableId();
            Long sequence = patchedCheckInForm.getSequence().isPresent() ? patchedCheckInForm.getSequence().get() : actualDocument.getSequence();
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTENCE_BY_TABLE_ID_AND_ACCOUNT_ID_AND_SEQUENCE.getValue(), tableId, accountId, sequence);
            boolean sameDocumentSw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateDocumentSw =  repository.existsByAccountIdAndTableIdAndSequence(accountId, tableId, sequence);
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTS_BY_TABLE_ID_AND_ACCOUNT_ID_AND_SEQUENCE.getValue(), tableId, accountId, sequence);
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_EXISTS, new Object[]{ "tableId: " + tableId,
                        ", accountId: " + accountId + ", sequence: " + sequence });
            }
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_NON_EXISTENCE_BY_TABLE_ID_AND_ACCOUNT_ID_AND_SEQUENCE.getValue(), tableId, accountId, sequence);

        }
    }

    private void checkUniquenessOfCheckIn(CheckInForm checkInForm, CheckInDocument actualDocument) throws CheckInException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(3);
        if(StringUtils.hasText(StringUtils.trimWhitespace(checkInForm.getAccountId()))) {
            similaritySwitchesCollection.add(checkInForm.getAccountId().compareTo(actualDocument.getAccountId()) == 0);
        }
        if(!ObjectUtils.isEmpty(checkInForm.getSequence())) {
            similaritySwitchesCollection.add(checkInForm.getSequence().compareTo(actualDocument.getSequence()) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(checkInForm.getTableId()))) {
            similaritySwitchesCollection.add(checkInForm.getTableId().compareTo(actualDocument.getTableId()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String accountId = StringUtils.hasText(StringUtils.trimWhitespace(checkInForm.getAccountId())) ? checkInForm.getAccountId() : actualDocument.getAccountId();
            Long sequence = !ObjectUtils.isEmpty(checkInForm.getSequence()) ? checkInForm.getSequence() : actualDocument.getSequence();
            String tableId = StringUtils.hasText(StringUtils.trimWhitespace(checkInForm.getTableId())) ? checkInForm.getTableId() : actualDocument.getTableId();
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTENCE_BY_TABLE_ID_AND_ACCOUNT_ID_AND_SEQUENCE.getValue(), tableId, accountId, sequence);
            boolean sameDocumentSw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateDocumentSw =  repository.existsByAccountIdAndTableIdAndSequence(accountId, tableId, sequence);
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTS_BY_TABLE_ID_AND_ACCOUNT_ID_AND_SEQUENCE.getValue(), tableId, accountId, sequence);
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_EXISTS, new Object[]{ "tableId: " + tableId,
                        ", accountId: " + accountId + ", sequence: " + sequence });
            }
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_NON_EXISTENCE_BY_TABLE_ID_AND_ACCOUNT_ID_AND_SEQUENCE.getValue(), tableId, accountId, sequence);

        }
    }
    
}