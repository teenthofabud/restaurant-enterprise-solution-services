package com.teenthofabud.restaurant.solution.engagement.checkin.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.github.fge.jsonpatch.JsonPatch;
import com.github.fge.jsonpatch.JsonPatchException;
import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.WalkInDto2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.WalkInEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.WalkInForm2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInMessageTemplate;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInDto;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.factory.CheckInBeanFactory;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.WalkInEntitySelfMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.WalkInForm2EntityMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.WalkInRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.WalkInService;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.WalkInDtoValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.WalkInFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.WalkInFormValidator;
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
import java.util.*;

@Slf4j
public class WalkInServiceImpl implements WalkInService {

    private TOABBaseService toabBaseService;
    private CheckInBeanFactory checkInBeanFactory;
    private EngagementServiceHelper engagementServiceHelper;
    private String walkInTimeFormat;

    @Override
    @Value("${res.engagement.checkIn.walkIn.timestamp.format}")
    public void setWalkInTimeFormat(String walkInTimeFormat) {
        this.walkInTimeFormat = walkInTimeFormat;
    }

    @Override
    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Override
    @Autowired
    public void setCheckInBeanFactory(CheckInBeanFactory checkInBeanFactory) {
        this.checkInBeanFactory = checkInBeanFactory;
    }

    @Override
    public String getContextCheckInType() {
        return CheckInType.WALK_IN.name();
    }

    @Override
    @Autowired
    public void setEngagementServiceHelper(EngagementServiceHelper engagementServiceHelper) {
        this.engagementServiceHelper = engagementServiceHelper;
    }

    @Override
    public WalkInFormValidator getCheckInFormValidator() {
        return (WalkInFormValidator) checkInBeanFactory.getCheckInFormValidator(getContextCheckInType()).get();
    }

    @Override
    public WalkInFormRelaxedValidator getCheckInFormRelaxedValidator() {
        return (WalkInFormRelaxedValidator) checkInBeanFactory.getCheckInFormRelaxedValidator(getContextCheckInType()).get();
    }

    @Override
    public WalkInDtoValidator getCheckInDtoValidator() {
        return (WalkInDtoValidator) checkInBeanFactory.getCheckInDtoValidator(getContextCheckInType()).get();
    }

    @Override
    public WalkInRepository getCheckInRepository() {
        return (WalkInRepository) checkInBeanFactory.getCheckInRepository(getContextCheckInType()).get();
    }

    @Override
    public WalkInEntitySelfMapper getCheckInEntitySelfMapper() {
        return (WalkInEntitySelfMapper) checkInBeanFactory.getCheckInEntitySelfMapper(getContextCheckInType()).get();
    }

    @Override
    public WalkInForm2EntityMapper getCheckInForm2EntityMapper() {
        return (WalkInForm2EntityMapper) checkInBeanFactory.getCheckInForm2EntityMapper(getContextCheckInType()).get();
    }

    @Override
    public WalkInForm2EntityConverter getCheckInForm2EntityConverter() {
        return (WalkInForm2EntityConverter) checkInBeanFactory.getCheckInForm2EntityConverter(getContextCheckInType()).get();
    }

    @Override
    public WalkInEntity2VoConverter getCheckInEntity2VoConverter() {
        return (WalkInEntity2VoConverter) checkInBeanFactory.getCheckInEntity2VoConverter(getContextCheckInType()).get();
    }

    @Override
    public WalkInDto2EntityConverter getCheckInDto2EntityConverter() {
        return (WalkInDto2EntityConverter) checkInBeanFactory.getCheckInDto2EntityConverter(getContextCheckInType()).get();
    }
    @Override
    public Comparator<WalkInVo> getCheckInVoTypeComparator() {
        return new Comparator<WalkInVo>() {
            @Override
            public int compare(WalkInVo s1, WalkInVo s2) {
                int c1 = s1.getAccountId().compareTo(s2.getAccountId());
                if (c1 == 0) {
                    int c2 = s1.getSequence().compareTo(s2.getSequence());
                    if (c2 == 0) {
                        int c3 = s1.getPhoneNumber().compareTo(s2.getPhoneNumber());
                        return c3;
                    } else {
                        return c2;
                    }
                } else {
                    return c1;
                }
            }
        };
    }

    @Override
    public Set<WalkInVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all WalkInEntity by their natural ordering");
        List<WalkInEntity> walkInEntityList = this.getCheckInRepository().findAll();
        List<WalkInVo> walkInVoList = engagementServiceHelper.walkInEntity2DetailedVo(walkInEntityList);
        Set<WalkInVo> naturallyOrderedSet = new TreeSet<>(getCheckInVoTypeComparator());
        naturallyOrderedSet.addAll(walkInVoList);
        log.info("{} WalkInVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public WalkInVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws CheckInException {
        log.info("Requesting WalkInEntity by id: {}", id);
        Long idL = this.parsePK(id);
        Optional<WalkInEntity> optEntity = this.getCheckInRepository().findById(idL);
        if(optEntity.isEmpty()) {
            log.debug("No WalkInEntity found by id: {}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found WalkInVo by id: {}", id);
        WalkInEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        WalkInVo vo = engagementServiceHelper.walkInEntity2DetailedVo(entity);
        log.debug("WalkInVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<WalkInVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalAccountId, Optional<String> optionalSequence, Optional<String> optionalNotes) throws CheckInException {
        if(optionalAccountId.isEmpty() && optionalSequence.isEmpty() && optionalNotes.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String accountId = optionalAccountId.isPresent() ? optionalAccountId.get() : "";
        String sequence = optionalSequence.isPresent() ? optionalSequence.get() : "";
        String notes = optionalNotes.isPresent() ? optionalNotes.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(accountId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(sequence)) && StringUtils.isEmpty(StringUtils.trimWhitespace(notes))) {
            log.debug("All search parameters are empty");
        }
        List<WalkInVo> matchedWalkInList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        WalkInEntity entity = new WalkInEntity();
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
        Example<WalkInEntity> walkInEntityExample = Example.of(entity, matcherCriteria);
        List<WalkInEntity> walkInEntityList = this.getCheckInRepository().findAll(walkInEntityExample);
        matchedWalkInList = engagementServiceHelper.walkInEntity2DetailedVo(walkInEntityList);
        log.info("Found {} WalkInVo matching with provided parameters : {}", matchedWalkInList.size(), providedFilters);
        log.info("No WalkInVo available matching with provided parameters : {}", matchedWalkInList.size(), providedFilters);
        return matchedWalkInList;
    }

    @Override
    public WalkInVo retrieveAllMatchingDetailsByCriteria(String sequence, String date) throws CheckInException {
        Long seq = 0l;
        LocalDate dt = LocalDate.now();
        LocalDateTime start = LocalDateTime.now();
        LocalDateTime end = LocalDateTime.now();
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(walkInTimeFormat);

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

        log.info("Requesting WalkInEntity by sequence: {} between timestamps: {} and {}", seq, start, end);
        Optional<WalkInEntity> optEntity = this.getCheckInRepository().findBySequenceAndCreatedOnBetween(seq, start, end);
        if(optEntity.isEmpty()) {
            log.debug("No WalkInEntity found by sequence: {} between timestamps: {} and {}", seq, start, end);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "seq: " + seq, ", date: " + date });
        }
        log.info("Found WalkInVo by sequence: {} between timestamps: {} and {}", seq, start, end);
        WalkInEntity entity = optEntity.get();
        WalkInVo vo = engagementServiceHelper.walkInEntity2DetailedVo(entity);
        return vo;
    }

    @Override
    public String createCheckIn(WalkInForm form) throws CheckInException {
        log.info("Creating new WalkInEntity");

        if(form == null) {
            log.debug("WalkInForm provided is null");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of WalkInForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        this.getCheckInFormValidator().validate(form, err);
        if(err.hasErrors()) {
            log.debug("WalkInForm has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("WalkInForm error detail: {}", ec);
            throw new CheckInException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of WalkInForm are valid");

        WalkInEntity expectedEntity = this.getCheckInForm2EntityConverter().convert(form);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), form.getAccountId(), form.getSequence());
        if(this.getCheckInRepository().existsByAccountIdAndSequence(expectedEntity.getAccountId(), expectedEntity.getSequence())) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(),
                    expectedEntity.getAccountId(), expectedEntity.getSequence());
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_EXISTS,
                    new Object[]{"accountId: " + form.getAccountId(), "sequence: " + form.getSequence() });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), expectedEntity.getAccountId(), expectedEntity.getSequence());

        log.debug("Saving {}", expectedEntity);
        WalkInEntity actualEntity = (WalkInEntity) this.getCheckInRepository().save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist WalkInForm details" });
        }
        log.info("Created new WalkInForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Override
    public void updateCheckIn(String id, WalkInForm form) throws CheckInException {
        log.info("Updating WalkInForm by id: {}", id);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<WalkInEntity> optActualEntity = this.getCheckInRepository().findById(idL);
        if(optActualEntity.isEmpty()) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        WalkInEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("WalkInEntity is inactive with id: {}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("WalkInEntity is active with id: {}", id);

        if(form == null) {
            log.debug("WalkInForm is null");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of WalkInForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = this.getCheckInFormRelaxedValidator().validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("WalkInForm has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("WalkInForm error detail: {}", ec);
            throw new CheckInException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of WalkInForm are empty");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of WalkInForm are valid");

        Optional<WalkInEntity> optExpectedEntity = this.getCheckInForm2EntityMapper().compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of WalkInForm");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from WalkInForm to WalkInEntity");

        WalkInEntity expectedEntity = optExpectedEntity.get();

        this.checkUniquenessOfCheckIn(form, actualEntity);

        this.getCheckInEntitySelfMapper().compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from WalkInEntity to WalkInForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = (WalkInEntity) this.getCheckInRepository().save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency checkIn details" });
        }
        log.info("Updated existing WalkInEntity with id: {}", actualEntity.getId());
    }

    @Override
    public void deleteCheckIn(String id) throws CheckInException {
        log.info("Soft deleting WalkInEntity by id: {}", id);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<WalkInEntity> optEntity = this.getCheckInRepository().findById(idL);
        if(optEntity.isEmpty()) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        WalkInEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("WalkInEntity is inactive with id: {}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("WalkInEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        WalkInEntity expectedEntity = (WalkInEntity) this.getCheckInRepository().save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current checkIn details with id:" + id });
        }

        log.info("Soft deleted existing WalkInEntity with id: {}", actualEntity.getId());

    }

    @Override
    public void applyPatchOnCheckIn(String id, List<PatchOperationForm> patches) throws CheckInException {
        log.info("Patching WalkInEntity by id: {}", id);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<WalkInEntity> optActualEntity = this.getCheckInRepository().findById(idL);
        if(optActualEntity.isEmpty()) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        WalkInEntity actualEntity = optActualEntity.get();
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


        log.debug("Patching list items to WalkInDto");
        WalkInDto patchedWalkInForm = new WalkInDto();
        try {
            log.debug("Preparing patch list items for CheckIn");
            JsonNode walkInDtoTree = OBJECT_MAPPER.convertValue(patches, JsonNode.class);
            JsonPatch checkInPatch = JsonPatch.fromJson(walkInDtoTree);
            log.debug("Prepared patch list items for CheckIn");
            JsonNode blankWalkInDtoTree = OBJECT_MAPPER.convertValue(new WalkInDto(), JsonNode.class);
            JsonNode patchedWalkInFormTree = checkInPatch.apply(blankWalkInDtoTree);
            log.debug("Applying patch list items to WalkInDto");
            patchedWalkInForm = OBJECT_MAPPER.treeToValue(patchedWalkInFormTree, WalkInDto.class);
            log.debug("Applied patch list items to WalkInDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to WalkInDto: {}", e);
            CheckInException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in WalkInDto");
                ex = new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to WalkInDto: {}", e);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to WalkInDto");

        log.debug("Validating patched WalkInDto");
        Errors err = new DirectFieldBindingResult(patchedWalkInForm, patchedWalkInForm.getClass().getSimpleName());
        this.getCheckInDtoValidator().validate(patchedWalkInForm, err);
        if(err.hasErrors()) {
            log.debug("Patched WalkInDto has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched WalkInDto error detail: {}", ec);
            throw new CheckInException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched WalkInDto are valid");

        this.checkUniquenessOfCheckIn(patchedWalkInForm, actualEntity);

        log.debug("Comparatively copying patched attributes from WalkInDto to WalkInEntity");
        try {
            this.getCheckInDto2EntityConverter().compareAndMap(patchedWalkInForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (CheckInException) e;
        }
        log.debug("Comparatively copied patched attributes from WalkInDto to WalkInEntity");

        log.debug("Saving patched WalkInEntity: {}", actualEntity);
        actualEntity = (WalkInEntity) this.getCheckInRepository().save(actualEntity);
        log.debug("Saved patched WalkInEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete WalkInEntity with id:{}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency checkIn details with id:" + id });
        }
        log.info("Patched WalkInEntity with id:{}", id);
    }

    private void checkUniquenessOfCheckIn(WalkInDto patchedWalkInForm, WalkInEntity actualEntity) throws CheckInException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(2);
        if(patchedWalkInForm.getAccountId().isPresent()) {
            similaritySwitchesCollection.add(patchedWalkInForm.getAccountId().get().compareTo(actualEntity.getAccountId()) == 0);
        }
        if(patchedWalkInForm.getSequence().isPresent()) {
            similaritySwitchesCollection.add(patchedWalkInForm.getSequence().get().compareTo(actualEntity.getSequence()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String accountId = patchedWalkInForm.getAccountId().isPresent() ? patchedWalkInForm.getAccountId().get() : actualEntity.getAccountId();
            String sequence = patchedWalkInForm.getSequence().isPresent() ? patchedWalkInForm.getSequence().get() : actualEntity.getSequence();
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

    private void checkUniquenessOfCheckIn(WalkInForm walkInForm, WalkInEntity actualEntity) throws CheckInException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(3);
        if(StringUtils.hasText(StringUtils.trimWhitespace(walkInForm.getAccountId()))) {
            similaritySwitchesCollection.add(walkInForm.getAccountId().compareTo(actualEntity.getAccountId()) == 0);
        }
        if(!ObjectUtils.isEmpty(walkInForm.getSequence())) {
            similaritySwitchesCollection.add(walkInForm.getSequence().compareTo(actualEntity.getSequence()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String accountId = StringUtils.hasText(StringUtils.trimWhitespace(walkInForm.getAccountId())) ? walkInForm.getAccountId() : actualEntity.getAccountId();
            String sequence = !ObjectUtils.isEmpty(walkInForm.getSequence()) ? walkInForm.getSequence() : actualEntity.getSequence();
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

    @Override
    public List<WalkInVo> retrieveAllMatchingWalkInDetailsByCriteria(Optional<String> optionalName, Optional<String> optionalPhoneNumber, Optional<String> optionalEmailId) throws CheckInException {
        if(optionalName.isEmpty() && optionalPhoneNumber.isEmpty() && optionalEmailId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String phoneNumber = optionalPhoneNumber.isPresent() ? optionalPhoneNumber.get() : "";
        String emailId = optionalEmailId.isPresent() ? optionalEmailId.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(phoneNumber)) && StringUtils.isEmpty(StringUtils.trimWhitespace(emailId))) {
            log.debug("All search parameters are empty");
        }
        List<WalkInVo> matchedWalkInList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        WalkInEntity entity = new WalkInEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            log.debug("name {} is valid", name);
            providedFilters.put("name", name);
            entity.setName(name);
            matcherCriteria = matcherCriteria.withMatcher("name", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(phoneNumber))) {
            log.debug("phoneNumber {} is valid", name);
            providedFilters.put("phoneNumber", name);
            entity.setPhoneNumber(phoneNumber);
            matcherCriteria = matcherCriteria.withMatcher("phoneNumber", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(emailId))) {
            log.debug("emailId {} is valid", name);
            providedFilters.put("emailId", name);
            entity.setEmailId(emailId);
            matcherCriteria = matcherCriteria.withMatcher("emailId", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<WalkInEntity> walkInEntityExample = Example.of(entity, matcherCriteria);
        List<WalkInEntity> walkInEntityList = this.getCheckInRepository().findAll(walkInEntityExample);
        matchedWalkInList = this.engagementServiceHelper.walkInEntity2DetailedVo(walkInEntityList);
        log.info("Found {} WalkInVo matching with provided parameters : {}", matchedWalkInList.size(), providedFilters);
        log.info("No WalkInVo available matching with provided parameters : {}", matchedWalkInList.size(), providedFilters);
        return matchedWalkInList;
    }
}