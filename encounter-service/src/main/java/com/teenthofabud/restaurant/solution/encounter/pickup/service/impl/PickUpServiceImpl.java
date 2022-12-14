package com.teenthofabud.restaurant.solution.encounter.pickup.service.impl;

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
import com.teenthofabud.restaurant.solution.encounter.meeting.constants.MeetingType;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingMessageTemplate;
import com.teenthofabud.restaurant.solution.encounter.meeting.factory.MeetingBeanFactory;
import com.teenthofabud.restaurant.solution.encounter.pickup.converter.PickUpDto2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.converter.PickUpEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.converter.PickUpForm2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpDto;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpEntity;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpForm;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpVo;
import com.teenthofabud.restaurant.solution.encounter.pickup.mapper.PickUpEntitySelfMapper;
import com.teenthofabud.restaurant.solution.encounter.pickup.mapper.PickUpForm2EntityMapper;
import com.teenthofabud.restaurant.solution.encounter.pickup.repository.PickUpRepository;
import com.teenthofabud.restaurant.solution.encounter.pickup.service.PickUpService;
import com.teenthofabud.restaurant.solution.encounter.pickup.validator.PickUpDtoValidator;
import com.teenthofabud.restaurant.solution.encounter.pickup.validator.PickUpFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.encounter.pickup.validator.PickUpFormValidator;
import com.teenthofabud.restaurant.solution.encounter.utils.EncounterServiceHelper;
import constants.EncounterErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
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
public class PickUpServiceImpl implements PickUpService {

    private TOABBaseService toabBaseService;
    private MeetingBeanFactory meetingBeanFactory;
    private EncounterServiceHelper encounterServiceHelper;

    private String pickUpTimeFormat;

    @Value("${res.encounter.meeting.timestamp.format}")
    public void setWalkInTimeFormat(String pickUpTimeFormat) {
        this.pickUpTimeFormat = pickUpTimeFormat;
    }

    @Autowired
    public void setMeetingBeanFactory(MeetingBeanFactory meetingBeanFactory) {
        this.meetingBeanFactory = meetingBeanFactory;
    }

    @Autowired
    public void setEncounterServiceHelper(EncounterServiceHelper encounterServiceHelper) {
        this.encounterServiceHelper = encounterServiceHelper;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }
    @Override
    public String getContextMeetingType() {
        return MeetingType.PICK_UP.name();
    }

    @Override
    public PickUpFormValidator getMeetingFormValidator() {
        return (PickUpFormValidator) this.meetingBeanFactory.getMeetingFormValidator(getContextMeetingType()).get();
    }

    @Override
    public PickUpFormRelaxedValidator getMeetingFormRelaxedValidator() {
        return (PickUpFormRelaxedValidator) this.meetingBeanFactory.getMeetingFormRelaxedValidator(getContextMeetingType()).get();
    }

    @Override
    public PickUpDtoValidator getMeetingDtoValidator() {
        return (PickUpDtoValidator) this.meetingBeanFactory.getMeetingDtoValidator(getContextMeetingType()).get();
    }

    @Override
    public PickUpRepository getMeetingRepository() {
        return (PickUpRepository) this.meetingBeanFactory.getMeetingRepository(getContextMeetingType()).get();
    }

    @Override
    public PickUpEntitySelfMapper getMeetingEntitySelfMapper() {
        return (PickUpEntitySelfMapper) this.meetingBeanFactory.getMeetingEntitySelfMapper(getContextMeetingType()).get();
    }

    @Override
    public PickUpForm2EntityMapper getMeetingForm2EntityMapper() {
        return (PickUpForm2EntityMapper) this.meetingBeanFactory.getMeetingForm2EntityMapper(getContextMeetingType()).get();
    }

    @Override
    public PickUpForm2EntityConverter getMeetingForm2EntityConverter() {
        return (PickUpForm2EntityConverter) this.meetingBeanFactory.getMeetingForm2EntityConverter(getContextMeetingType()).get();
    }

    @Override
    public PickUpEntity2VoConverter getMeetingEntity2VoConverter() {
        return (PickUpEntity2VoConverter) this.meetingBeanFactory.getMeetingEntity2VoConverter(getContextMeetingType()).get();
    }

    @Override
    public PickUpDto2EntityConverter getMeetingDto2EntityConverter() {
        return (PickUpDto2EntityConverter) this.meetingBeanFactory.getMeetingDto2EntityConverter(getContextMeetingType()).get();
    }

    @Override
    public Comparator<PickUpVo> getMeetingVoTypeComparator() {
        return new Comparator<PickUpVo>() {
            @Override
            public int compare(PickUpVo s1, PickUpVo s2) {
                int c1 = s1.getAccountId().compareTo(s2.getAccountId());
                if (c1 == 0) {
                    int c2 = s1.getSequence().compareTo(s2.getSequence());
                    if (c2 == 0) {
                        return s1.getPhoneNo().compareTo(s2.getPhoneNo());
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
    public Set<PickUpVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all PickUpEntity by their natural ordering");
        List<PickUpEntity> deliveryEntityList = this.getMeetingRepository().findAll();
        List<PickUpVo> deliveryVoList = encounterServiceHelper.deliveryEntity2DetailedVo(deliveryEntityList);
        Set<PickUpVo> naturallyOrderedSet = new TreeSet<>(getMeetingVoTypeComparator());
        naturallyOrderedSet.addAll(deliveryVoList);
        log.info("{} PickUpVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public PickUpVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws MeetingException {
        log.info("Requesting PickUpEntity by id: {}", id);
        Long idL = this.parsePK(id);
        Optional<PickUpEntity> optEntity = this.getMeetingRepository().findById(idL);
        if(optEntity.isEmpty()) {
            log.debug("No PickUpEntity found by id: {}", id);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found PickUpVo by id: {}", id);
        PickUpEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        PickUpVo vo = encounterServiceHelper.pickUpEntity2DetailedVo(entity);
        log.debug("PickUpVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<PickUpVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalAccountId, Optional<String> optionalSequence) throws MeetingException {
        if(optionalAccountId.isEmpty() && optionalSequence.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String accountId = optionalAccountId.isPresent() ? optionalAccountId.get() : "";
        String sequence = optionalSequence.isPresent() ? optionalSequence.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(accountId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(sequence))) {
            log.debug("All search parameters are empty");
        }
        List<PickUpVo> matchedPickUpList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        PickUpEntity entity = new PickUpEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(accountId))) {
            log.debug("accountId {} is valid", accountId);
            providedFilters.put("accountId", accountId);
            entity.setAccountId(accountId);
            matcherCriteria = matcherCriteria.withMatcher("accountId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(sequence))) {
            log.debug("sequence {} is valid", sequence);
            providedFilters.put("sequence", sequence);
            entity.setSequence(sequence);
            matcherCriteria = matcherCriteria.withMatcher("sequence", match -> match.exact());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<PickUpEntity> deliveryEntityExample = Example.of(entity, matcherCriteria);
        List<PickUpEntity> deliveryEntityList = this.getMeetingRepository().findAll(deliveryEntityExample);
        matchedPickUpList = encounterServiceHelper.deliveryEntity2DetailedVo(deliveryEntityList);
        log.info("Found {} PickUpVo matching with provided parameters : {}", matchedPickUpList.size(), providedFilters);
        log.info("No PickUpVo available matching with provided parameters : {}", matchedPickUpList.size(), providedFilters);
        return matchedPickUpList;
    }

    @Override
    public PickUpVo retrieveMatchingDetailsBySequenceOnDate(String sequence, String date) throws MeetingException {
        Long seq = 0l;
        LocalDate dt = LocalDate.now();
        LocalDateTime start = LocalDateTime.now();
        LocalDateTime end = LocalDateTime.now();
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(pickUpTimeFormat);

        try {
            seq = Long.parseLong(sequence);
        } catch (NumberFormatException e) {
            log.debug("Sequence: {} format is invalid", sequence);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "sequence", sequence });
        }

        try {
            dt = LocalDate.parse(date, dtf);
        } catch (DateTimeParseException e) {
            log.debug("Date: {} format is invalid", date);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "date", date });
        }

        start = LocalDateTime.of(dt, LocalTime.of(0,0, 0));
        end = LocalDateTime.of(dt, LocalTime.of(23,59, 59));

        log.info("Requesting PickUpEntity by sequence: {} between timestamps: {} and {}", seq, start, end);
        Optional<PickUpEntity> optEntity = this.getMeetingRepository().findBySequenceAndCreatedOnBetween(seq, start, end);
        if(optEntity.isEmpty()) {
            log.debug("No PickUpEntity found by sequence: {} between timestamps: {} and {}", seq, start, end);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "seq: " + seq, ", date: " + date });
        }
        log.info("Found PickUpVo by sequence: {} between timestamps: {} and {}", seq, start, end);
        PickUpEntity entity = optEntity.get();
        PickUpVo vo = encounterServiceHelper.pickUpEntity2DetailedVo(entity);
        return vo;
    }

    @Override
    public String createMeeting(PickUpForm form) throws MeetingException {
        log.info("Creating new PickUpEntity");

        if(form == null) {
            log.debug("PickUpForm provided is null");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of PickUpForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        this.getMeetingFormValidator().validate(form, err);
        if(err.hasErrors()) {
            log.debug("PickUpForm has {} errors", err.getErrorCount());
            EncounterErrorCode ec = EncounterErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("PickUpForm error detail: {}", ec);
            throw new MeetingException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of PickUpForm are valid");

        PickUpEntity expectedEntity = this.getMeetingForm2EntityConverter().convert(form);

        log.debug("Saving {}", expectedEntity);
        PickUpEntity actualEntity = this.getMeetingRepository().save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist PickUpForm details" });
        }
        log.info("Created new PickUpForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Override
    public void updateMeeting(String id, PickUpForm form) throws MeetingException {
        log.info("Updating PickUpForm by id: {}", id);

        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_MEETING_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<PickUpEntity> optActualEntity = this.getMeetingRepository().findById(idL);
        if(optActualEntity.isEmpty()) {
            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_NO_MEETING_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_FOUND_MEETING_ENTITY_ID.getValue(), id);

        PickUpEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("PickUpEntity is inactive with id: {}", id);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("PickUpEntity is active with id: {}", id);

        if(form == null) {
            log.debug("PickUpForm is null");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of PickUpForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = this.getMeetingFormRelaxedValidator().validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("PickUpForm has {} errors", err.getErrorCount());
            EncounterErrorCode ec = EncounterErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("PickUpForm error detail: {}", ec);
            throw new MeetingException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of PickUpForm are empty");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of PickUpForm are valid");

        Optional<PickUpEntity> optExpectedEntity = this.getMeetingForm2EntityMapper().compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of PickUpForm");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from PickUpForm to PickUpEntity");

        PickUpEntity expectedEntity = optExpectedEntity.get();

        this.checkUniquenessOfMeeting(form, actualEntity);

        this.getMeetingEntitySelfMapper().compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from PickUpEntity to PickUpForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = (PickUpEntity) this.getMeetingRepository().save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency checkIn details" });
        }
        log.info("Updated existing PickUpEntity with id: {}", actualEntity.getId());
    }

    @Override
    public void deleteMeeting(String id) throws MeetingException {
        log.info("Soft deleting PickUpEntity by id: {}", id);

        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_MEETING_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<PickUpEntity> optEntity = this.getMeetingRepository().findById(idL);
        if(optEntity.isEmpty()) {
            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_NO_MEETING_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_FOUND_MEETING_ENTITY_ID.getValue(), id);

        PickUpEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("PickUpEntity is inactive with id: {}", id);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("PickUpEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        PickUpEntity expectedEntity = (PickUpEntity) this.getMeetingRepository().save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current checkIn details with id:" + id });
        }

        log.info("Soft deleted existing PickUpEntity with id: {}", actualEntity.getId());
    }

    @Override
    public void applyPatchOnMeeting(String id, List<PatchOperationForm> patches) throws MeetingException {
        log.info("Patching PickUpEntity by id: {}", id);

        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_MEETING_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<PickUpEntity> optActualEntity = this.getMeetingRepository().findById(idL);
        if(optActualEntity.isEmpty()) {
            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_NO_MEETING_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_FOUND_MEETING_ENTITY_ID.getValue(), id);

        PickUpEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Meeting patch list not provided");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Meeting patch list has {} items", patches.size());


        log.debug("Validating patch list items for Meeting");
        try {
            toabBaseService.validatePatches(patches, EncounterErrorCode.ENCOUNTER_EXISTS.getDomain() + ":LOV");
            log.debug("All Meeting patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Meeting patch item are invalid");
            throw new MeetingException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Meeting");


        log.debug("Patching list items to PickUpDto");
        PickUpDto patchedPickUpForm = new PickUpDto();
        try {
            log.debug("Preparing patch list items for Meeting");
            JsonNode deliveryDtoTree = OBJECT_MAPPER.convertValue(patches, JsonNode.class);
            JsonPatch checkInPatch = JsonPatch.fromJson(deliveryDtoTree);
            log.debug("Prepared patch list items for Meeting");
            JsonNode blankPickUpDtoTree = OBJECT_MAPPER.convertValue(new PickUpDto(), JsonNode.class);
            JsonNode patchedPickUpFormTree = checkInPatch.apply(blankPickUpDtoTree);
            log.debug("Applying patch list items to PickUpDto");
            patchedPickUpForm = OBJECT_MAPPER.treeToValue(patchedPickUpFormTree, PickUpDto.class);
            log.debug("Applied patch list items to PickUpDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to PickUpDto: {}", e);
            MeetingException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in PickUpDto");
                ex = new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to PickUpDto: {}", e);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to PickUpDto");

        log.debug("Validating patched PickUpDto");
        Errors err = new DirectFieldBindingResult(patchedPickUpForm, patchedPickUpForm.getClass().getSimpleName());
        this.getMeetingDtoValidator().validate(patchedPickUpForm, err);
        if(err.hasErrors()) {
            log.debug("Patched PickUpDto has {} errors", err.getErrorCount());
            EncounterErrorCode ec = EncounterErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched PickUpDto error detail: {}", ec);
            throw new MeetingException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched PickUpDto are valid");

        this.checkUniquenessOfMeeting(patchedPickUpForm, actualEntity);

        log.debug("Comparatively copying patched attributes from PickUpDto to PickUpEntity");
        try {
            this.getMeetingDto2EntityConverter().compareAndMap(patchedPickUpForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (MeetingException) e;
        }
        log.debug("Comparatively copied patched attributes from PickUpDto to PickUpEntity");

        log.debug("Saving patched PickUpEntity: {}", actualEntity);
        actualEntity = (PickUpEntity) this.getMeetingRepository().save(actualEntity);
        log.debug("Saved patched PickUpEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete PickUpEntity with id:{}", id);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency checkIn details with id:" + id });
        }
        log.info("Patched PickUpEntity with id:{}", id);
    }

    @Override
    public List<PickUpVo> retrieveAllMatchingPickUpDetailsByCriteria(Optional<String> optionalName, Optional<String> optionalPhoneNumber)
            throws MeetingException {
        if(optionalName.isEmpty() && optionalPhoneNumber.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String phoneNumber = optionalPhoneNumber.isPresent() ? optionalPhoneNumber.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(phoneNumber))) {
            log.debug("All search parameters are empty");
        }
        List<PickUpVo> matchedPickUpList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        PickUpEntity entity = new PickUpEntity(new MeetingEntity());
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
            entity.setPhoneNo(phoneNumber);
            matcherCriteria = matcherCriteria.withMatcher("phoneNumber", match -> match.exact());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<PickUpEntity> pickUpEntityExample = Example.of(entity, matcherCriteria);
        List<PickUpEntity> pickUpEntityList = this.getMeetingRepository().findAll(pickUpEntityExample);
        matchedPickUpList = encounterServiceHelper.pickUpEntity2DetailedVo(pickUpEntityList);
        log.info("Found {} PickUpVo matching with provided parameters : {}", matchedPickUpList.size(), providedFilters);
        log.info("No PickUpVo available matching with provided parameters : {}", matchedPickUpList.size(), providedFilters);
        return matchedPickUpList;
    }
}