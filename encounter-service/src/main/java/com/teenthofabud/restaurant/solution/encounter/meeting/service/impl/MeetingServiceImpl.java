//package com.teenthofabud.restaurant.solution.encounter.meeting.service.impl;
//
//import com.fasterxml.jackson.databind.JsonNode;
//import com.fasterxml.jackson.databind.ObjectMapper;
//import com.github.fge.jsonpatch.JsonPatch;
//import com.github.fge.jsonpatch.JsonPatchException;
//import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
//import com.teenthofabud.core.common.constant.TOABCascadeLevel;
//import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
//import com.teenthofabud.core.common.data.form.PatchOperationForm;
//import com.teenthofabud.core.common.error.TOABBaseException;
//import com.teenthofabud.core.common.error.TOABSystemException;
//import com.teenthofabud.core.common.service.TOABBaseService;
//import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingDto2EntityConverter;
//import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingEntity2VoConverter;
//import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingForm2EntityConverter;
//import com.teenthofabud.restaurant.solution.encounter.meeting.data.*;
//import com.teenthofabud.restaurant.solution.encounter.meeting.factory.MeetingBeanFactory;
//import com.teenthofabud.restaurant.solution.encounter.meeting.mapper.MeetingEntitySelfMapper;
//import com.teenthofabud.restaurant.solution.encounter.meeting.mapper.MeetingForm2EntityMapper;
//import com.teenthofabud.restaurant.solution.encounter.meeting.repository.MeetingRepository;
//import com.teenthofabud.restaurant.solution.encounter.meeting.service.MeetingService;
//import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingDtoValidator;
//import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingFormRelaxedValidator;
//import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingFormValidator;
//import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpVo;
//import com.teenthofabud.restaurant.solution.encounter.utils.EncounterServiceHelper;
//import constants.EncounterErrorCode;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.beans.factory.annotation.Value;
//import org.springframework.data.domain.Example;
//import org.springframework.data.domain.ExampleMatcher;
//import org.springframework.stereotype.Component;
//import org.springframework.util.ObjectUtils;
//import org.springframework.util.StringUtils;
//import org.springframework.validation.DirectFieldBindingResult;
//import org.springframework.validation.Errors;
//
//import java.io.IOException;
//import java.time.LocalDate;
//import java.time.LocalDateTime;
//import java.time.LocalTime;
//import java.time.ZoneOffset;
//import java.time.format.DateTimeFormatter;
//import java.time.format.DateTimeParseException;
//import java.util.*;
//
//@Slf4j
//public abstract class MeetingServiceImpl<A extends MeetingFormValidator, B extends MeetingFormRelaxedValidator,
//        C extends MeetingDtoValidator, D extends MeetingRepository, E extends MeetingEntitySelfMapper,
//        F extends MeetingForm2EntityMapper, G extends MeetingForm2EntityConverter, H extends MeetingEntity2VoConverter,
//        I extends MeetingDto2EntityConverter> implements MeetingService<MeetingForm, MeetingVo> {
//
//    private static final Comparator<MeetingVo> CMP_BY_ACCOUNT_SEQUENCE = (s1, s2) -> {
//       int c1 = s1.getAccountId().compareTo(s2.getAccountId());
//       if(c1 == 0) {
//           int c2 = s1.getSequence().compareTo(s2.getSequence());
//           return c2;
//       } else {
//           return c1;
//       }
//    };
//
//    private TOABBaseService toabBaseService;
//    private ObjectMapper om;
//    protected EncounterServiceHelper encounterServiceHelper;
//    protected MeetingBeanFactory meetingBeanFactory;
//    private String meetingTimeFormat;
//
//    @Value("${res.encounter.meeting.timestamp}")
//    public void setCheckInTimeFormat(String meetingTimeFormat) {
//        this.meetingTimeFormat = meetingTimeFormat;
//    }
//
//    @Autowired
//    public void setEncounterServiceHelper(EncounterServiceHelper encounterServiceHelper) {
//        this.encounterServiceHelper = encounterServiceHelper;
//    }
//
//    @Autowired
//    public void setToabBaseService(TOABBaseService toabBaseService) {
//        this.toabBaseService = toabBaseService;
//    }
//
//    @Autowired
//    public void setOm(ObjectMapper om) {
//        this.om = om;
//    }
//
//    @Autowired
//    public void setMeetingBeanFactory(MeetingBeanFactory meetingBeanFactory) {
//        this.meetingBeanFactory = meetingBeanFactory;
//    }
//
//    private Long parsePK(String id) throws MeetingException {
//        Long meetingId = -1L;
//        try {
//            meetingId = Long.parseLong(id);
//            log.debug("Parsed id {} to meeting id {} in numeric format", id, meetingId);
//            if(meetingId <= 0) {
//                throw new NumberFormatException("meeting id can't be zero/negative");
//            }
//        } catch (NumberFormatException e) {
//            log.error("Unable to parse meeting id", e);
//            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_ID_INVALID.getValue(), id);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "id", id });
//        }
//        return meetingId;
//    }
//
//    @Override
//    public Set<MeetingVo> retrieveAllByNaturalOrdering() {
//        log.info("Requesting all MeetingEntity by their natural ordering");
//        List<MeetingEntity> meetingEntityList = this.getMeetingRepository().findAll();
//        List<MeetingVo> meetingVoList = encounterServiceHelper.meetingEntity2DetailedVo(meetingEntityList);
//        Set<MeetingVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_ACCOUNT_SEQUENCE);
//        naturallyOrderedSet.addAll(meetingVoList);
//        log.info("{} MeetingVo available", naturallyOrderedSet.size());
//        return naturallyOrderedSet;
//    }
//
//    @Override
//    public MeetingVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws MeetingException {
//        log.info("Requesting MeetingEntity by id: {}", id);
//        Long idL = this.parsePK(id);
//        Optional<MeetingEntity> optEntity = this.getMeetingRepository().findById(idL);
//        if(optEntity.isEmpty()) {
//            log.debug("No MeetingEntity found by id: {}", id);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
//        }
//        log.info("Found MeetingVo by id: {}", id);
//        MeetingEntity entity = optEntity.get();
//        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
//        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
//        MeetingVo vo = encounterServiceHelper.meetingEntity2DetailedVo(entity);
//        log.debug("MeetingVo populated with fields cascaded to level: {}", cascadeLevel);
//        TOABRequestContextHolder.clearCascadeLevelContext();
//        return vo;
//    }
//    @Override
//    public List<MeetingVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalAccountId, Optional<String> optionalSequence) throws MeetingException {
//        if(optionalAccountId.isEmpty() && optionalSequence.isEmpty()) {
//            log.debug("No search parameters provided");
//        }
//        String accountId = optionalAccountId.isPresent() ? optionalAccountId.get() : "";
//        String sequence = optionalSequence.isPresent() ? optionalSequence.get() : "";
//        if(StringUtils.isEmpty(StringUtils.trimWhitespace(accountId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(sequence))) {
//            log.debug("All search parameters are empty");
//        }
//        List<MeetingVo> matchedMeetingList = new LinkedList<>();
//        Map<String, String> providedFilters = new LinkedHashMap<>();
//        MeetingEntity entity = new MeetingEntity();
//        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
//        if(StringUtils.hasText(StringUtils.trimWhitespace(accountId))) {
//            log.debug("accountId {} is valid", accountId);
//            providedFilters.put("accountId", accountId);
//            entity.setAccountId(accountId);
//            matcherCriteria = matcherCriteria.withMatcher("accountId", match -> match.exact());
//        }
//        if(StringUtils.hasText(StringUtils.trimWhitespace(sequence))) {
//            log.debug("sequence {} is valid", accountId);
//            providedFilters.put("sequence", accountId);
//            entity.setSequence(sequence);
//            matcherCriteria = matcherCriteria.withMatcher("sequence", match -> match.exact());
//        }
//        if(providedFilters.isEmpty()) {
//            log.debug("search parameters are not valid");
//        } else {
//            log.debug("search parameters {} are valid", providedFilters);
//        }
//        Example<MeetingEntity> meetingEntityExample = Example.of(entity, matcherCriteria);
//        List<MeetingEntity> meetingEntityList = this.getMeetingRepository().findAll(meetingEntityExample);
//        matchedMeetingList = encounterServiceHelper.meetingEntity2DetailedVo(meetingEntityList);
//        log.info("Found {} MeetingVo matching with provided parameters : {}", matchedMeetingList.size(), providedFilters);
//        log.info("No MeetingVo available matching with provided parameters : {}", matchedMeetingList.size(), providedFilters);
//        return matchedMeetingList;
//    }
//
//    @Override
//    public MeetingVo retrieveAllMatchingDetailsByCriteria(String sequence, String date) throws MeetingException {
//        Long seq = 0l;
//        LocalDate dt = LocalDate.now();
//        LocalDateTime start = LocalDateTime.now();
//        LocalDateTime end = LocalDateTime.now();
//        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(meetingTimeFormat);
//
//        try {
//            seq = Long.parseLong(sequence);
//        } catch (NumberFormatException e) {
//            log.debug("Sequence: {} format is invalid", sequence);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "sequence", sequence });
//        }
//
//        try {
//            dt = LocalDate.parse(date, dtf);
//        } catch (DateTimeParseException e) {
//            log.debug("Date: {} format is invalid", dtf);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "date", dtf });
//        }
//
//        start = LocalDateTime.of(dt, LocalTime.of(0,0, 0));
//        end = LocalDateTime.of(dt, LocalTime.of(23,59, 59));
//
//        log.info("Requesting MeetingEntity by sequence: {} between timestamps: {} and {}", seq, start, end);
//        Optional<MeetingEntity> optEntity = this.getMeetingRepository().findBySequenceAndCreatedOnBetween(seq, start, end);
//        if(optEntity.isEmpty()) {
//            log.debug("No MeetingEntity found by sequence: {} between timestamps: {} and {}", seq, start, end);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "seq: " + seq, ", date: " + date });
//        }
//        log.info("Found MeetingVo by sequence: {} between timestamps: {} and {}", seq, start, end);
//        MeetingEntity entity = optEntity.get();
//        MeetingVo vo = encounterServiceHelper.meetingEntity2DetailedVo(entity);
//        return vo;
//    }
//
//    @Override
//    public String createMeeting(MeetingForm form) throws MeetingException {
//        log.info("Creating new MeetingEntity");
//
//        if(form == null) {
//            log.debug("MeetingForm provided is null");
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED,
//                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
//        }
//        log.debug("Form details: {}", form);
//
//        log.debug("Validating provided attributes of MeetingForm");
//        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
//        this.getMeetingFormValidator().validate(form, err);
//        if(err.hasErrors()) {
//            log.debug("MeetingForm has {} errors", err.getErrorCount());
//            EncounterErrorCode ec = EncounterErrorCode.valueOf(err.getFieldError().getCode());
//            log.debug("MeetingForm error detail: {}", ec);
//            throw new MeetingException(ec, new Object[] { err.getFieldError().getField() });
//        }
//        log.debug("All attributes of MeetingForm are valid");
//
//        MeetingEntity expectedEntity = this.getMeetingForm2EntityConverter().convert(form);
//
//        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), form.getAccountId(), form.getSequence());
//        if(this.getMeetingRepository().existsByAccountIdAndSequence(expectedEntity.getAccountId(), expectedEntity.getSequence())) {
//            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(),
//                    expectedEntity.getAccountId(), expectedEntity.getSequence());
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_EXISTS,
//                    new Object[]{"accountId: " + form.getAccountId(), "sequence: " + form.getSequence() });
//        }
//        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), expectedEntity.getAccountId(), expectedEntity.getSequence());
//
//        log.debug("Saving {}", expectedEntity);
//        MeetingEntity actualEntity = (MeetingEntity) this.getMeetingRepository().save(expectedEntity);
//        log.debug("Saved {}", actualEntity);
//
//        if(actualEntity == null) {
//            log.debug("Unable to create {}", expectedEntity);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE,
//                    new Object[]{ "creation", "unable to persist MeetingForm details" });
//        }
//        log.info("Created new MeetingForm with id: {}", actualEntity.getId());
//        return actualEntity.getId().toString();
//    }
//
//    @Override
//    public void updateMeeting(String id, MeetingForm form) throws MeetingException {
//        log.info("Updating MeetingForm by id: {}", id);
//
//        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_MEETING_ENTITY_ID.getValue(), id);
//        Long idL = this.parsePK(id);
//        Optional<MeetingEntity> optActualEntity = this.getMeetingRepository().findById(idL);
//        if(optActualEntity.isEmpty()) {
//            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_NO_MEETING_ENTITY_ID_AVAILABLE.getValue(), id);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
//        }
//        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_FOUND_MEETING_ENTITY_ID.getValue(), id);
//
//        MeetingEntity actualEntity = optActualEntity.get();
//        if(!actualEntity.getActive()) {
//            log.debug("MeetingEntity is inactive with id: {}", id);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_INACTIVE, new Object[] { String.valueOf(id) });
//        }
//        log.debug("MeetingEntity is active with id: {}", id);
//
//        if(form == null) {
//            log.debug("MeetingForm is null");
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
//        }
//        log.debug("Form details : {}", form);
//
//        log.debug("Validating provided attributes of MeetingForm");
//        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
//        Boolean allEmpty = this.getMeetingFormRelaxedValidator().validateLoosely(form, err);
//        if(err.hasErrors()) {
//            log.debug("MeetingForm has {} errors", err.getErrorCount());
//            EncounterErrorCode ec = EncounterErrorCode.valueOf(err.getFieldError().getCode());
//            log.debug("MeetingForm error detail: {}", ec);
//            throw new MeetingException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
//        } else if (!allEmpty) {
//            log.debug("All attributes of MeetingForm are empty");
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
//        }
//        log.debug("All attributes of MeetingForm are valid");
//
//        Optional<MeetingEntity> optExpectedEntity = this.getMeetingForm2EntityMapper().compareAndMap(actualEntity, form);
//        if(optExpectedEntity.isEmpty()) {
//            log.debug("No new value for attributes of MeetingForm");
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
//        }
//        log.debug("Successfully compared and copied attributes from MeetingForm to MeetingEntity");
//
//        MeetingEntity expectedEntity = optExpectedEntity.get();
//
//        this.checkUniquenessOfMeeting(form, actualEntity);
//
//        this.getMeetingEntitySelfMapper().compareAndMap(expectedEntity, actualEntity);
//        log.debug("Compared and copied attributes from MeetingEntity to MeetingForm");
//        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
//
//        log.debug("Updating: {}", actualEntity);
//        actualEntity = (MeetingEntity) this.getMeetingRepository().save(actualEntity);
//        log.debug("Updated: {}", actualEntity);
//        if(actualEntity == null) {
//            log.debug("Unable to update {}", actualEntity);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE,
//                    new Object[]{ "update", "unable to persist currency meeting details" });
//        }
//        log.info("Updated existing MeetingEntity with id: {}", actualEntity.getId());
//    }
//
//    @Override
//    public void deleteMeeting(String id) throws MeetingException {
//        log.info("Soft deleting MeetingEntity by id: {}", id);
//
//        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_MEETING_ENTITY_ID.getValue(), id);
//        Long idL = this.parsePK(id);
//        Optional<MeetingEntity> optEntity = this.getMeetingRepository().findById(idL);
//        if(optEntity.isEmpty()) {
//            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_NO_MEETING_ENTITY_ID_AVAILABLE.getValue(), id);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
//        }
//        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_FOUND_MEETING_ENTITY_ID.getValue(), id);
//
//        MeetingEntity actualEntity = optEntity.get();
//        if(!actualEntity.getActive()) {
//            log.debug("MeetingEntity is inactive with id: {}", id);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_INACTIVE, new Object[] { String.valueOf(id) });
//        }
//        log.debug("MeetingEntity is active with id: {}", id);
//
//        actualEntity.setActive(Boolean.FALSE);
//        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
//        log.debug("Soft deleting: {}", actualEntity);
//        MeetingEntity expectedEntity = (MeetingEntity) this.getMeetingRepository().save(actualEntity);
//        log.debug("Soft deleted: {}", expectedEntity);
//        if(expectedEntity == null) {
//            log.debug("Unable to soft delete {}", actualEntity);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE,
//                    new Object[]{ "deletion", "unable to soft delete current meeting details with id:" + id });
//        }
//
//        log.info("Soft deleted existing MeetingEntity with id: {}", actualEntity.getId());
//    }
//
//    @Override
//    public void applyPatchOnMeeting(String id, List<PatchOperationForm> patches) throws MeetingException {
//        log.info("Patching MeetingEntity by id: {}", id);
//
//        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_MEETING_ENTITY_ID.getValue(), id);
//        Long idL = this.parsePK(id);
//        Optional<MeetingEntity> optActualEntity = this.getMeetingRepository().findById(idL);
//        if(optActualEntity.isEmpty()) {
//            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_NO_MEETING_ENTITY_ID_AVAILABLE.getValue(), id);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
//        }
//        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_FOUND_MEETING_ENTITY_ID.getValue(), id);
//
//        MeetingEntity actualEntity = optActualEntity.get();
//        if(patches == null || (patches != null && patches.isEmpty())) {
//            log.debug("Meeting patch list not provided");
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
//        }
//        log.debug("Meeting patch list has {} items", patches.size());
//
//
//        log.debug("Validating patch list items for Meeting");
//        try {
//            toabBaseService.validatePatches(patches, EncounterErrorCode.ENCOUNTER_EXISTS.getDomain() + ":LOV");
//            log.debug("All Meeting patch list items are valid");
//        } catch (TOABSystemException e) {
//            log.debug("Some of the Meeting patch item are invalid");
//            throw new MeetingException(e.getError(), e.getParameters());
//        }
//        log.debug("Validated patch list items for Meeting");
//
//
//        log.debug("Patching list items to MeetingDto");
//        MeetingDto patchedMeetingForm = new MeetingDto();
//        try {
//            log.debug("Preparing patch list items for Meeting");
//            JsonNode meetingDtoTree = om.convertValue(patches, JsonNode.class);
//            JsonPatch meetingPatch = JsonPatch.fromJson(meetingDtoTree);
//            log.debug("Prepared patch list items for Meeting");
//            JsonNode blankMeetingDtoTree = om.convertValue(new MeetingDto(), JsonNode.class);
//            JsonNode patchedMeetingFormTree = meetingPatch.apply(blankMeetingDtoTree);
//            log.debug("Applying patch list items to MeetingDto");
//            patchedMeetingForm = om.treeToValue(patchedMeetingFormTree, MeetingDto.class);
//            log.debug("Applied patch list items to MeetingDto");
//        } catch (JsonPatchException e) {
//            log.debug("Failed to patch list items to MeetingDto: {}", e);
//            MeetingException ex = null;
//            if(e.getMessage().contains("no such path in target")) {
//                log.debug("Invalid patch attribute in MeetingDto");
//                ex = new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[]{ "path" });
//            } else {
//                ex = new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
//            }
//            throw ex;
//        } catch (IOException e) {
//            log.debug("Failed to patch list items to MeetingDto: {}", e);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
//        }
//        log.debug("Successfully to patch list items to MeetingDto");
//
//        log.debug("Validating patched MeetingDto");
//        Errors err = new DirectFieldBindingResult(patchedMeetingForm, patchedMeetingForm.getClass().getSimpleName());
//        this.getMeetingDtoValidator().validate(patchedMeetingForm, err);
//        if(err.hasErrors()) {
//            log.debug("Patched MeetingDto has {} errors", err.getErrorCount());
//            EncounterErrorCode ec = EncounterErrorCode.valueOf(err.getFieldError().getCode());
//            log.debug("Patched MeetingDto error detail: {}", ec);
//            throw new MeetingException(ec, new Object[] { err.getFieldError().getField() });
//        }
//        log.debug("All attributes of patched MeetingDto are valid");
//
//        this.checkUniquenessOfMeeting(patchedMeetingForm, actualEntity);
//
//        log.debug("Comparatively copying patched attributes from MeetingDto to MeetingEntity");
//        try {
//            this.getMeetingDto2EntityConverter().compareAndMap(patchedMeetingForm, actualEntity);
//        } catch (TOABBaseException e) {
//            throw (MeetingException) e;
//        }
//        log.debug("Comparatively copied patched attributes from MeetingDto to MeetingEntity");
//
//        log.debug("Saving patched MeetingEntity: {}", actualEntity);
//        actualEntity = (MeetingEntity) this.getMeetingRepository().save(actualEntity);
//        log.debug("Saved patched MeetingEntity: {}", actualEntity);
//        if(actualEntity == null) {
//            log.debug("Unable to patch delete MeetingEntity with id:{}", id);
//            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE,
//                    new Object[]{ "patching", "unable to patch currency meeting details with id:" + id });
//        }
//        log.info("Patched MeetingEntity with id:{}", id);
//    }
//
//    private void checkUniquenessOfMeeting(MeetingDto patchedMeetingForm, MeetingEntity actualEntity) throws MeetingException {
//        List<Boolean> similaritySwitchesCollection = new ArrayList<>(2);
//        if(patchedMeetingForm.getAccountId().isPresent()) {
//            similaritySwitchesCollection.add(patchedMeetingForm.getAccountId().get().compareTo(actualEntity.getAccountId()) == 0);
//        }
//        if(patchedMeetingForm.getSequence().isPresent()) {
//            similaritySwitchesCollection.add(patchedMeetingForm.getSequence().get().compareTo(actualEntity.getSequence()) == 0);
//        }
//        if(!similaritySwitchesCollection.isEmpty()) {
//            String accountId = patchedMeetingForm.getAccountId().isPresent() ? patchedMeetingForm.getAccountId().get() : actualEntity.getAccountId();
//            String sequence = patchedMeetingForm.getSequence().isPresent() ? patchedMeetingForm.getSequence().get() : actualEntity.getSequence();
//            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);
//            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
//            boolean duplicateEntitySw =  this.getMeetingRepository().existsByAccountIdAndSequence(accountId, sequence);
//            if(sameEntitySw || duplicateEntitySw) {
//                log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);
//                throw new MeetingException(EncounterErrorCode.ENCOUNTER_EXISTS, new Object[]{ "accountId: " + accountId, "sequence: " + sequence });
//            }
//            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);
//
//        }
//    }
//
//    private void checkUniquenessOfMeeting(MeetingForm meetingForm, MeetingEntity actualEntity) throws MeetingException {
//        List<Boolean> similaritySwitchesCollection = new ArrayList<>(3);
//        if(StringUtils.hasText(StringUtils.trimWhitespace(meetingForm.getAccountId()))) {
//            similaritySwitchesCollection.add(meetingForm.getAccountId().compareTo(actualEntity.getAccountId()) == 0);
//        }
//        if(!ObjectUtils.isEmpty(meetingForm.getSequence())) {
//            similaritySwitchesCollection.add(meetingForm.getSequence().compareTo(actualEntity.getSequence()) == 0);
//        }
//        if(!similaritySwitchesCollection.isEmpty()) {
//            String accountId = StringUtils.hasText(StringUtils.trimWhitespace(meetingForm.getAccountId())) ? meetingForm.getAccountId() : actualEntity.getAccountId();
//            String sequence = !ObjectUtils.isEmpty(meetingForm.getSequence()) ? meetingForm.getSequence() : actualEntity.getSequence();
//            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);
//            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
//            boolean duplicateEntitySw =  this.getMeetingRepository().existsByAccountIdAndSequence(accountId, sequence);
//            if(sameEntitySw || duplicateEntitySw) {
//                log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);
//                throw new MeetingException(EncounterErrorCode.ENCOUNTER_EXISTS, new Object[]{ "accountId: " + accountId, "sequence: " + sequence });
//            }
//            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), accountId, sequence);
//
//        }
//    }
//
//    public abstract A getMeetingFormValidator();
//    public abstract B getMeetingFormRelaxedValidator();
//    public abstract C getMeetingDtoValidator();
//    public abstract D getMeetingRepository();
//    public abstract E getMeetingEntitySelfMapper();
//    public abstract F getMeetingForm2EntityMapper();
//    public abstract G getMeetingForm2EntityConverter();
//    public abstract H getMeetingEntity2VoConverter();
//    public abstract I getMeetingDto2EntityConverter();
//}