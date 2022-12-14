/*
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
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.TableAllocationDto2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.TableAllocationEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.TableAllocationForm2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.TableAllocationEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.TableAllocationDto;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.TableAllocationException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.TableAllocationForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.TableAllocationMessageTemplate;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.TableAllocationVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.factory.CheckInBeanFactory;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.TableAllocationEntitySelfMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.TableAllocationForm2EntityMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.CheckInRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.TableAllocationService;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.TableAllocationDtoValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.TableAllocationFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.TableAllocationFormValidator;
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
public abstract class CheckInServiceImpl<A extends TableAllocationFormValidator, B extends TableAllocationFormRelaxedValidator, C extends TableAllocationDtoValidator, D extends CheckInRepository,
        E extends TableAllocationEntitySelfMapper, F extends TableAllocationForm2EntityMapper, G extends TableAllocationForm2EntityConverter,
        H extends TableAllocationEntity2VoConverter, I extends TableAllocationDto2EntityConverter> implements TableAllocationService<TableAllocationForm, TableAllocationVo> {

    private static final Comparator<TableAllocationVo> CMP_BY_ACCOUNT_SEQUENCE = (s1, s2) -> {
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

    private Long parsePK(String id) throws TableAllocationException {
        Long checkInId = -1L;
        try {
            checkInId = Long.parseLong(id);
            log.debug("Parsed id {} to checkIn id {} in numeric format", id, checkInId);
            if(checkInId <= 0) {
                throw new NumberFormatException("checkIn id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse checkIn id", e);
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_CHECKIN_ID_INVALID.getValue(), id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return checkInId;
    }

    @Override
    public Set<TableAllocationVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all TableAllocationEntity by their natural ordering");
        List<TableAllocationEntity> checkInEntityList = this.getCheckInRepository().findAll();
        List<TableAllocationVo> checkInVoList = engagementServiceHelper.checkInEntity2DetailedVo(checkInEntityList);
        Set<TableAllocationVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_ACCOUNT_SEQUENCE);
        naturallyOrderedSet.addAll(checkInVoList);
        log.info("{} TableAllocationVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public TableAllocationVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws TableAllocationException {
        log.info("Requesting TableAllocationEntity by id: {}", id);
        Long idL = this.parsePK(id);
        Optional<TableAllocationEntity> optEntity = this.getCheckInRepository().findById(idL);
        if(optEntity.isEmpty()) {
            log.debug("No TableAllocationEntity found by id: {}", id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found TableAllocationVo by id: {}", id);
        TableAllocationEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        TableAllocationVo vo = engagementServiceHelper.checkInEntity2DetailedVo(entity);
        log.debug("TableAllocationVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }
    @Override
    public List<TableAllocationVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalAccountId, Optional<String> optionalSequence, Optional<String> optionalNotes) throws TableAllocationException {
        if(optionalAccountId.isEmpty() && optionalSequence.isEmpty() && optionalNotes.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String accountId = optionalAccountId.isPresent() ? optionalAccountId.get() : "";
        String sequence = optionalSequence.isPresent() ? optionalSequence.get() : "";
        String notes = optionalNotes.isPresent() ? optionalNotes.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(accountId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(sequence)) && StringUtils.isEmpty(StringUtils.trimWhitespace(notes))) {
            log.debug("All search parameters are empty");
        }
        List<TableAllocationVo> matchedCheckInList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        TableAllocationEntity entity = new TableAllocationEntity();
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
        Example<TableAllocationEntity> checkInEntityExample = Example.of(entity, matcherCriteria);
        List<TableAllocationEntity> checkInEntityList = this.getCheckInRepository().findAll(checkInEntityExample);
        matchedCheckInList = engagementServiceHelper.checkInEntity2DetailedVo(checkInEntityList);
        log.info("Found {} TableAllocationVo matching with provided parameters : {}", matchedCheckInList.size(), providedFilters);
        log.info("No TableAllocationVo available matching with provided parameters : {}", matchedCheckInList.size(), providedFilters);
        return matchedCheckInList;
    }

    @Override
    public TableAllocationVo retrieveAllMatchingDetailsByCriteria(String sequence, String date) throws TableAllocationException {
        Long seq = 0l;
        LocalDate dt = LocalDate.now();
        LocalDateTime start = LocalDateTime.now();
        LocalDateTime end = LocalDateTime.now();
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(checkInTimeFormat);

        try {
            seq = Long.parseLong(sequence);
        } catch (NumberFormatException e) {
            log.debug("Sequence: {} format is invalid", sequence);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "sequence", sequence });
        }

        try {
            dt = LocalDate.parse(date, dtf);
        } catch (DateTimeParseException e) {
            log.debug("Date: {} format is invalid", sequence);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "sequence", sequence });
        }

        start = LocalDateTime.of(dt, LocalTime.of(0,0, 0));
        end = LocalDateTime.of(dt, LocalTime.of(23,59, 59));

        log.info("Requesting TableAllocationEntity by sequence: {} between timestamps: {} and {}", seq, start, end);
        Optional<TableAllocationEntity> optEntity = this.getCheckInRepository().findBySequenceAndCreatedOnBetween(seq, start, end);
        if(optEntity.isEmpty()) {
            log.debug("No TableAllocationEntity found by sequence: {} between timestamps: {} and {}", seq, start, end);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "seq: " + seq, ", date: " + date });
        }
        log.info("Found TableAllocationVo by sequence: {} between timestamps: {} and {}", seq, start, end);
        TableAllocationEntity entity = optEntity.get();
        TableAllocationVo vo = engagementServiceHelper.checkInEntity2DetailedVo(entity);
        return vo;
    }

    @Override
    public String createCheckIn(TableAllocationForm form) throws TableAllocationException {
        log.info("Creating new TableAllocationEntity");

        if(form == null) {
            log.debug("TableAllocationForm provided is null");
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of TableAllocationForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        this.getCheckInFormValidator().validate(form, err);
        if(err.hasErrors()) {
            log.debug("TableAllocationForm has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TableAllocationForm error detail: {}", ec);
            throw new TableAllocationException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of TableAllocationForm are valid");

        TableAllocationEntity expectedEntity = this.getCheckInForm2EntityConverter().convert(form);

        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), form.getAccountId(), form.getSequence());
        if(this.getCheckInRepository().existsByAccountIdAndSequence(expectedEntity.getAccountId(), expectedEntity.getSequence())) {
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(),
                    expectedEntity.getAccountId(), expectedEntity.getSequence());
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_EXISTS,
                    new Object[]{"accountId: " + form.getAccountId(), "sequence: " + form.getSequence() });
        }
        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_CHECKIN_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), expectedEntity.getAccountId(), expectedEntity.getSequence());

        log.debug("Saving {}", expectedEntity);
        TableAllocationEntity actualEntity = (TableAllocationEntity) this.getCheckInRepository().save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist TableAllocationForm details" });
        }
        log.info("Created new TableAllocationForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Override
    public void updateCheckIn(String id, TableAllocationForm form) throws TableAllocationException {
        log.info("Updating TableAllocationForm by id: {}", id);

        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<TableAllocationEntity> optActualEntity = this.getCheckInRepository().findById(idL);
        if(optActualEntity.isEmpty()) {
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        TableAllocationEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TableAllocationEntity is inactive with id: {}", id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TableAllocationEntity is active with id: {}", id);

        if(form == null) {
            log.debug("TableAllocationForm is null");
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of TableAllocationForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = this.getCheckInFormRelaxedValidator().validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("TableAllocationForm has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TableAllocationForm error detail: {}", ec);
            throw new TableAllocationException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of TableAllocationForm are empty");
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of TableAllocationForm are valid");

        Optional<TableAllocationEntity> optExpectedEntity = this.getCheckInForm2EntityMapper().compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of TableAllocationForm");
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from TableAllocationForm to TableAllocationEntity");

        TableAllocationEntity expectedEntity = optExpectedEntity.get();

        this.checkUniquenessOfCheckIn(form, actualEntity);

        this.getCheckInEntitySelfMapper().compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from TableAllocationEntity to TableAllocationForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = (TableAllocationEntity) this.getCheckInRepository().save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency checkIn details" });
        }
        log.info("Updated existing TableAllocationEntity with id: {}", actualEntity.getId());
    }

    @Override
    public void deleteCheckIn(String id) throws TableAllocationException {
        log.info("Soft deleting TableAllocationEntity by id: {}", id);

        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<TableAllocationEntity> optEntity = this.getCheckInRepository().findById(idL);
        if(optEntity.isEmpty()) {
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        TableAllocationEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TableAllocationEntity is inactive with id: {}", id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TableAllocationEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        TableAllocationEntity expectedEntity = (TableAllocationEntity) this.getCheckInRepository().save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current checkIn details with id:" + id });
        }

        log.info("Soft deleted existing TableAllocationEntity with id: {}", actualEntity.getId());
    }

    @Override
    public void applyPatchOnCheckIn(String id, List<PatchOperationForm> patches) throws TableAllocationException {
        log.info("Patching TableAllocationEntity by id: {}", id);

        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<TableAllocationEntity> optActualEntity = this.getCheckInRepository().findById(idL);
        if(optActualEntity.isEmpty()) {
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        TableAllocationEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("CheckIn patch list not provided");
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("CheckIn patch list has {} items", patches.size());


        log.debug("Validating patch list items for CheckIn");
        try {
            toabBaseService.validatePatches(patches, EngagementErrorCode.ENGAGEMENT_EXISTS.getDomain() + ":LOV");
            log.debug("All CheckIn patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the CheckIn patch item are invalid");
            throw new TableAllocationException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for CheckIn");


        log.debug("Patching list items to TableAllocationDto");
        TableAllocationDto patchedCheckInForm = new TableAllocationDto();
        try {
            log.debug("Preparing patch list items for CheckIn");
            JsonNode checkInDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch checkInPatch = JsonPatch.fromJson(checkInDtoTree);
            log.debug("Prepared patch list items for CheckIn");
            JsonNode blankCheckInDtoTree = om.convertValue(new TableAllocationDto(), JsonNode.class);
            JsonNode patchedCheckInFormTree = checkInPatch.apply(blankCheckInDtoTree);
            log.debug("Applying patch list items to TableAllocationDto");
            patchedCheckInForm = om.treeToValue(patchedCheckInFormTree, TableAllocationDto.class);
            log.debug("Applied patch list items to TableAllocationDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to TableAllocationDto: {}", e);
            TableAllocationException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in TableAllocationDto");
                ex = new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to TableAllocationDto: {}", e);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to TableAllocationDto");

        log.debug("Validating patched TableAllocationDto");
        Errors err = new DirectFieldBindingResult(patchedCheckInForm, patchedCheckInForm.getClass().getSimpleName());
        this.getCheckInDtoValidator().validate(patchedCheckInForm, err);
        if(err.hasErrors()) {
            log.debug("Patched TableAllocationDto has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched TableAllocationDto error detail: {}", ec);
            throw new TableAllocationException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched TableAllocationDto are valid");

        this.checkUniquenessOfCheckIn(patchedCheckInForm, actualEntity);

        log.debug("Comparatively copying patched attributes from TableAllocationDto to TableAllocationEntity");
        try {
            this.getCheckInDto2EntityConverter().compareAndMap(patchedCheckInForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (TableAllocationException) e;
        }
        log.debug("Comparatively copied patched attributes from TableAllocationDto to TableAllocationEntity");

        log.debug("Saving patched TableAllocationEntity: {}", actualEntity);
        actualEntity = (TableAllocationEntity) this.getCheckInRepository().save(actualEntity);
        log.debug("Saved patched TableAllocationEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete TableAllocationEntity with id:{}", id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency checkIn details with id:" + id });
        }
        log.info("Patched TableAllocationEntity with id:{}", id);
    }

    private void checkUniquenessOfCheckIn(TableAllocationDto patchedCheckInForm, TableAllocationEntity actualEntity) throws TableAllocationException {
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
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  this.getCheckInRepository().existsByAccountIdAndSequence(accountId, sequence);
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);
                throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_EXISTS, new Object[]{ "accountId: " + accountId, "sequence: " + sequence });
            }
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_CHECKIN_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);

        }
    }

    private void checkUniquenessOfCheckIn(TableAllocationForm checkInForm, TableAllocationEntity actualEntity) throws TableAllocationException {
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
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  this.getCheckInRepository().existsByAccountIdAndSequence(accountId, sequence);
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);
                throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_EXISTS, new Object[]{ "accountId: " + accountId, "sequence: " + sequence });
            }
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_CHECKIN_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);

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
    
}*/
