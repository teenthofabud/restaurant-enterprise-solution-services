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
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.CheckInDto2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.CheckInEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.CheckInForm2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInDto;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInMessageTemplate;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.factory.CheckInBeanFactory;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.CheckInEntitySelfMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.CheckInForm2EntityMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.CheckInRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.CheckInService;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.CheckInDtoValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.CheckInFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.CheckInFormValidator;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import com.teenthofabud.restaurant.solution.engagement.utils.EngagementServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
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

@Slf4j
public abstract class CheckInServiceImpl<A extends CheckInFormValidator, B extends CheckInFormRelaxedValidator, C extends CheckInDtoValidator, D extends CheckInRepository,
        E extends CheckInEntitySelfMapper, F extends CheckInForm2EntityMapper, G extends CheckInForm2EntityConverter,
        H extends CheckInEntity2VoConverter, I extends CheckInDto2EntityConverter> implements CheckInService<CheckInForm, CheckInVo> {

    private static final Comparator<CheckInVo> CMP_BY_ACCOUNT_SEQUENCE = (s1, s2) -> {
       int c1 = s1.getAccountId().compareTo(s2.getAccountId());
       if(c1 == 0) {
           int c2 = s1.getSequence().compareTo(s2.getSequence());
           return c2;
       } else {
           return c1;
       }
    };

    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    protected EngagementServiceHelper engagementServiceHelper;
    protected CheckInBeanFactory checkInBeanFactory;
    private String checkInTimeFormat;

    @Value("${res.engagement.checkIn.timestamp}")
    public void setCheckInTimeFormat(String checkInTimeFormat) {
        this.checkInTimeFormat = checkInTimeFormat;
    }

    @Autowired
    public void setEngagementServiceHelper(EngagementServiceHelper engagementServiceHelper) {
        this.engagementServiceHelper = engagementServiceHelper;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setCheckInBeanFactory(CheckInBeanFactory checkInBeanFactory) {
        this.checkInBeanFactory = checkInBeanFactory;
    }

    private Long parsePK(String id) throws CheckInException {
        Long checkInId = -1L;
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
        log.info("Requesting all CheckInEntity by their natural ordering");
        List<CheckInEntity> checkInEntityList = this.getCheckInRepository().findAll();
        List<CheckInVo> checkInVoList = engagementServiceHelper.checkInEntity2DetailedVo(checkInEntityList);
        Set<CheckInVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_ACCOUNT_SEQUENCE);
        naturallyOrderedSet.addAll(checkInVoList);
        log.info("{} CheckInVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public CheckInVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws CheckInException {
        log.info("Requesting CheckInEntity by id: {}", id);
        Long idL = this.parsePK(id);
        Optional<CheckInEntity> optEntity = this.getCheckInRepository().findById(idL);
        if(optEntity.isEmpty()) {
            log.debug("No CheckInEntity found by id: {}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found CheckInVo by id: {}", id);
        CheckInEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        CheckInVo vo = engagementServiceHelper.checkInEntity2DetailedVo(entity);
        log.debug("CheckInVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }
    @Override
    public List<CheckInVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalAccountId, Optional<String> optionalSequence, Optional<String> optionalNotes) throws CheckInException {
        if(optionalAccountId.isEmpty() && optionalSequence.isEmpty() && optionalNotes.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String accountId = optionalAccountId.isPresent() ? optionalAccountId.get() : "";
        String sequence = optionalSequence.isPresent() ? optionalSequence.get() : "";
        String notes = optionalNotes.isPresent() ? optionalNotes.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(accountId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(sequence)) && StringUtils.isEmpty(StringUtils.trimWhitespace(notes))) {
            log.debug("All search parameters are empty");
        }
        List<CheckInVo> matchedCheckInList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        CheckInEntity entity = new CheckInEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(accountId))) {
            log.debug("accountId {} is valid", accountId);
            providedFilters.put("accountId", accountId);
            entity.setAccountId(accountId);
            matcherCriteria = matcherCriteria.withMatcher("accountId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(sequence))) {
            log.debug("sequence {} is valid", accountId);
            providedFilters.put("sequence", accountId);
            entity.setSequence(sequence);
            matcherCriteria = matcherCriteria.withMatcher("sequence", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(notes))) {
            log.debug("notes {} is valid", accountId);
            providedFilters.put("notes", accountId);
            entity.setNotes(notes);
            matcherCriteria = matcherCriteria.withMatcher("notes", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<CheckInEntity> checkInEntityExample = Example.of(entity, matcherCriteria);
        List<CheckInEntity> checkInEntityList = this.getCheckInRepository().findAll(checkInEntityExample);
        matchedCheckInList = engagementServiceHelper.checkInEntity2DetailedVo(checkInEntityList);
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
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "sequence", sequence });
        }

        try {
            dt = LocalDate.parse(date, dtf);
        } catch (DateTimeParseException e) {
            log.debug("Date: {} format is invalid", sequence);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "sequence", sequence });
        }

        start = LocalDateTime.of(dt, LocalTime.of(0,0, 0));
        end = LocalDateTime.of(dt, LocalTime.of(23,59, 59));

        log.info("Requesting CheckInEntity by sequence: {} between timestamps: {} and {}", seq, start, end);
        Optional<CheckInEntity> optEntity = this.getCheckInRepository().findBySequenceAndCreatedOnBetween(seq, start, end);
        if(optEntity.isEmpty()) {
            log.debug("No CheckInEntity found by sequence: {} between timestamps: {} and {}", seq, start, end);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "seq: " + seq, ", date: " + date });
        }
        log.info("Found CheckInVo by sequence: {} between timestamps: {} and {}", seq, start, end);
        CheckInEntity entity = optEntity.get();
        CheckInVo vo = engagementServiceHelper.checkInEntity2DetailedVo(entity);
        return vo;
    }

    @Override
    public String createCheckIn(CheckInForm form) throws CheckInException {
        log.info("Creating new CheckInEntity");

        if(form == null) {
            log.debug("CheckInForm provided is null");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of CheckInForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        this.getCheckInFormValidator().validate(form, err);
        if(err.hasErrors()) {
            log.debug("CheckInForm has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("CheckInForm error detail: {}", ec);
            throw new CheckInException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of CheckInForm are valid");

        CheckInEntity expectedEntity = this.getCheckInForm2EntityConverter().convert(form);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), form.getAccountId(), form.getSequence());
        if(this.getCheckInRepository().existsByAccountIdAndSequence(expectedEntity.getAccountId(), expectedEntity.getSequence())) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(),
                    expectedEntity.getAccountId(), expectedEntity.getSequence());
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_EXISTS,
                    new Object[]{"accountId: " + form.getAccountId(), "sequence: " + form.getSequence() });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), expectedEntity.getAccountId(), expectedEntity.getSequence());

        log.debug("Saving {}", expectedEntity);
        CheckInEntity actualEntity = (CheckInEntity) this.getCheckInRepository().save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist CheckInForm details" });
        }
        log.info("Created new CheckInForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Override
    public void updateCheckIn(String id, CheckInForm form) throws CheckInException {
        log.info("Updating CheckInForm by id: {}", id);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<CheckInEntity> optActualEntity = this.getCheckInRepository().findById(idL);
        if(optActualEntity.isEmpty()) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        CheckInEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("CheckInEntity is inactive with id: {}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("CheckInEntity is active with id: {}", id);

        if(form == null) {
            log.debug("CheckInForm is null");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of CheckInForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = this.getCheckInFormRelaxedValidator().validateLoosely(form, err);
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

        Optional<CheckInEntity> optExpectedEntity = this.getCheckInForm2EntityMapper().compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of CheckInForm");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from CheckInForm to CheckInEntity");

        CheckInEntity expectedEntity = optExpectedEntity.get();

        this.checkUniquenessOfCheckIn(form, actualEntity);

        this.getCheckInEntitySelfMapper().compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from CheckInEntity to CheckInForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = (CheckInEntity) this.getCheckInRepository().save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency checkIn details" });
        }
        log.info("Updated existing CheckInEntity with id: {}", actualEntity.getId());
    }

    @Override
    public void deleteCheckIn(String id) throws CheckInException {
        log.info("Soft deleting CheckInEntity by id: {}", id);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<CheckInEntity> optEntity = this.getCheckInRepository().findById(idL);
        if(optEntity.isEmpty()) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        CheckInEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("CheckInEntity is inactive with id: {}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("CheckInEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        CheckInEntity expectedEntity = (CheckInEntity) this.getCheckInRepository().save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current checkIn details with id:" + id });
        }

        log.info("Soft deleted existing CheckInEntity with id: {}", actualEntity.getId());
    }

    @Override
    public void applyPatchOnCheckIn(String id, List<PatchOperationForm> patches) throws CheckInException {
        log.info("Patching CheckInEntity by id: {}", id);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<CheckInEntity> optActualEntity = this.getCheckInRepository().findById(idL);
        if(optActualEntity.isEmpty()) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        CheckInEntity actualEntity = optActualEntity.get();
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
        this.getCheckInDtoValidator().validate(patchedCheckInForm, err);
        if(err.hasErrors()) {
            log.debug("Patched CheckInDto has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched CheckInDto error detail: {}", ec);
            throw new CheckInException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched CheckInDto are valid");

        this.checkUniquenessOfCheckIn(patchedCheckInForm, actualEntity);

        log.debug("Comparatively copying patched attributes from CheckInDto to CheckInEntity");
        try {
            this.getCheckInDto2EntityConverter().compareAndMap(patchedCheckInForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (CheckInException) e;
        }
        log.debug("Comparatively copied patched attributes from CheckInDto to CheckInEntity");

        log.debug("Saving patched CheckInEntity: {}", actualEntity);
        actualEntity = (CheckInEntity) this.getCheckInRepository().save(actualEntity);
        log.debug("Saved patched CheckInEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete CheckInEntity with id:{}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency checkIn details with id:" + id });
        }
        log.info("Patched CheckInEntity with id:{}", id);
    }

    private void checkUniquenessOfCheckIn(CheckInDto patchedCheckInForm, CheckInEntity actualEntity) throws CheckInException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(2);
        if(patchedCheckInForm.getAccountId().isPresent()) {
            similaritySwitchesCollection.add(patchedCheckInForm.getAccountId().get().compareTo(actualEntity.getAccountId()) == 0);
        }
        if(patchedCheckInForm.getSequence().isPresent()) {
            similaritySwitchesCollection.add(patchedCheckInForm.getSequence().get().compareTo(actualEntity.getSequence()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String accountId = patchedCheckInForm.getAccountId().isPresent() ? patchedCheckInForm.getAccountId().get() : actualEntity.getAccountId();
            String sequence = patchedCheckInForm.getSequence().isPresent() ? patchedCheckInForm.getSequence().get() : actualEntity.getSequence();
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  this.getCheckInRepository().existsByAccountIdAndSequence(accountId, sequence);
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_EXISTS, new Object[]{ "accountId: " + accountId, "sequence: " + sequence });
            }
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);

        }
    }

    private void checkUniquenessOfCheckIn(CheckInForm checkInForm, CheckInEntity actualEntity) throws CheckInException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(3);
        if(StringUtils.hasText(StringUtils.trimWhitespace(checkInForm.getAccountId()))) {
            similaritySwitchesCollection.add(checkInForm.getAccountId().compareTo(actualEntity.getAccountId()) == 0);
        }
        if(!ObjectUtils.isEmpty(checkInForm.getSequence())) {
            similaritySwitchesCollection.add(checkInForm.getSequence().compareTo(actualEntity.getSequence()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String accountId = StringUtils.hasText(StringUtils.trimWhitespace(checkInForm.getAccountId())) ? checkInForm.getAccountId() : actualEntity.getAccountId();
            String sequence = !ObjectUtils.isEmpty(checkInForm.getSequence()) ? checkInForm.getSequence() : actualEntity.getSequence();
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  this.getCheckInRepository().existsByAccountIdAndSequence(accountId, sequence);
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_EXISTS, new Object[]{ "accountId: " + accountId, "sequence: " + sequence });
            }
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);

        }
    }

    public abstract A getCheckInFormValidator();
    public abstract B getCheckInFormRelaxedValidator();
    public abstract C getCheckInDtoValidator();
    public abstract D getCheckInRepository();
    public abstract E getCheckInEntitySelfMapper();
    public abstract F getCheckInForm2EntityMapper();
    public abstract G getCheckInForm2EntityConverter();
    public abstract H getCheckInEntity2VoConverter();
    public abstract I getCheckInDto2EntityConverter();
    
}