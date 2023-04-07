package com.teenthofabud.restaurant.solution.encounter.delivery.service.impl;

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
import com.teenthofabud.restaurant.solution.encounter.delivery.converter.DeliveryDto2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.delivery.converter.DeliveryEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.delivery.converter.DeliveryForm2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryDto;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryEntity;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryForm;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryVo;
import com.teenthofabud.restaurant.solution.encounter.delivery.mapper.DeliveryEntitySelfMapper;
import com.teenthofabud.restaurant.solution.encounter.delivery.mapper.DeliveryForm2EntityMapper;
import com.teenthofabud.restaurant.solution.encounter.delivery.repository.DeliveryRepository;
import com.teenthofabud.restaurant.solution.encounter.delivery.service.DeliveryService;
import com.teenthofabud.restaurant.solution.encounter.delivery.validator.DeliveryDtoValidator;
import com.teenthofabud.restaurant.solution.encounter.delivery.validator.DeliveryFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.encounter.delivery.validator.DeliveryFormValidator;
import com.teenthofabud.restaurant.solution.encounter.meeting.constants.MeetingType;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingMessageTemplate;
import com.teenthofabud.restaurant.solution.encounter.meeting.factory.MeetingBeanFactory;
import com.teenthofabud.restaurant.solution.encounter.utils.EncounterServiceHelper;
import com.teenthofabud.restaurant.solution.encounter.constants.EncounterErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.math.NumberUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
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
public class DeliveryServiceImpl implements DeliveryService{

    private TOABBaseService toabBaseService;
    private MeetingBeanFactory meetingBeanFactory;
    private EncounterServiceHelper encounterServiceHelper;

    private String deliveryTimeFormat;

    @Value("${res.encounter.meeting.date.format}")
    public void setDeliveryTimeFormat(String deliveryTimeFormat) {
        this.deliveryTimeFormat = deliveryTimeFormat;
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
        return MeetingType.DELIVERY.name();
    }

    @Override
    public DeliveryFormValidator getMeetingFormValidator() {
        return (DeliveryFormValidator) this.meetingBeanFactory.getMeetingFormValidator(getContextMeetingType()).get();
    }

    @Override
    public DeliveryFormRelaxedValidator getMeetingFormRelaxedValidator() {
        return (DeliveryFormRelaxedValidator) this.meetingBeanFactory.getMeetingFormRelaxedValidator(getContextMeetingType()).get();
    }

    @Override
    public DeliveryDtoValidator getMeetingDtoValidator() {
        return (DeliveryDtoValidator) this.meetingBeanFactory.getMeetingDtoValidator(getContextMeetingType()).get();
    }

    @Override
    public DeliveryRepository getMeetingRepository() {
        return (DeliveryRepository) this.meetingBeanFactory.getMeetingRepository(getContextMeetingType()).get();
    }

    @Override
    public DeliveryEntitySelfMapper getMeetingEntitySelfMapper() {
        return (DeliveryEntitySelfMapper) this.meetingBeanFactory.getMeetingEntitySelfMapper(getContextMeetingType()).get();
    }

    @Override
    public DeliveryForm2EntityMapper getMeetingForm2EntityMapper() {
        return (DeliveryForm2EntityMapper) this.meetingBeanFactory.getMeetingForm2EntityMapper(getContextMeetingType()).get();
    }

    @Override
    public DeliveryForm2EntityConverter getMeetingForm2EntityConverter() {
        return (DeliveryForm2EntityConverter) this.meetingBeanFactory.getMeetingForm2EntityConverter(getContextMeetingType()).get();
    }

    @Override
    public DeliveryEntity2VoConverter getMeetingEntity2VoConverter() {
        return (DeliveryEntity2VoConverter) this.meetingBeanFactory.getMeetingEntity2VoConverter(getContextMeetingType()).get();
    }

    @Override
    public DeliveryDto2EntityConverter getMeetingDto2EntityConverter() {
        return (DeliveryDto2EntityConverter) this.meetingBeanFactory.getMeetingDto2EntityConverter(getContextMeetingType()).get();
    }

    @Override
    public Comparator<DeliveryVo> getMeetingVoTypeComparator() {
        return new Comparator<DeliveryVo>() {
            @Override
            public int compare(DeliveryVo s1, DeliveryVo s2) {
                int c1 = s1.getAccountId().compareTo(s2.getAccountId());
                if (c1 == 0) {
                    int c2 = s1.getSequence().compareTo(s2.getSequence());
                    if (c2 == 0) {
                        return s1.getOrderId().compareTo(s2.getOrderId());
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
    public List<DeliveryVo> retrieveAllMatchingDeliveryDetailsByCriteria(Optional<String> optionalOrderId) throws MeetingException {
        if (optionalOrderId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String orderId = optionalOrderId.isPresent() ? optionalOrderId.get() : "";
        if (StringUtils.isEmpty(StringUtils.trimWhitespace(orderId))) {
            log.debug("All search parameters are empty");
        }
        List<DeliveryVo> matchedDeliveryList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        DeliveryEntity entity = new DeliveryEntity(new MeetingEntity());
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll().withIgnorePaths("id", "sequence", "accountId").withIgnoreNullValues();
        if (StringUtils.hasText(StringUtils.trimWhitespace(orderId)) && NumberUtils.isDigits(StringUtils.trimWhitespace(orderId))) {
            log.debug("orderId {} is valid", orderId);
            providedFilters.put("orderId", orderId);
            entity.setOrderId(orderId);
            matcherCriteria = matcherCriteria.withMatcher("orderId", match -> match.exact());
        }
        if (providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID,
                    new Object[]{"orderId", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED});
        } else {
            log.debug("search parameters {} are valid", providedFilters);
            Example<DeliveryEntity> deliveryEntityExample = Example.of(entity, matcherCriteria);
            List<DeliveryEntity> deliveryEntityList = this.getMeetingRepository().findAll(deliveryEntityExample);
            matchedDeliveryList = encounterServiceHelper.deliveryEntity2DetailedVo(deliveryEntityList);
            if (!matchedDeliveryList.isEmpty())
                log.info("Found {} DeliveryVo matching with provided parameters : {}", matchedDeliveryList.size(), providedFilters);
            else
                log.info("No DeliveryVo available matching with provided parameters : {}", providedFilters);
            return matchedDeliveryList;
        }
    }

    @Override
    public Set<DeliveryVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all DeliveryEntity by their natural ordering");
        List<DeliveryEntity> deliveryEntityList = this.getMeetingRepository().findAll();
        List<DeliveryVo> deliveryVoList = encounterServiceHelper.deliveryEntity2DetailedVo(deliveryEntityList);
        Set<DeliveryVo> naturallyOrderedSet = new TreeSet<>(getMeetingVoTypeComparator());
        naturallyOrderedSet.addAll(deliveryVoList);
        log.info("{} DeliveryVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public DeliveryVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws MeetingException {
        log.info("Requesting DeliveryEntity by id: {}", id);
        Long idL = this.parsePK(id);
        Optional<DeliveryEntity> optEntity = this.getMeetingRepository().findById(idL);
        if(optEntity.isEmpty()) {
            log.debug("No DeliveryEntity found by id: {}", id);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found DeliveryVo by id: {}", id);
        DeliveryEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.orElse(TOABCascadeLevel.ZERO);
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        DeliveryVo vo = encounterServiceHelper.deliveryEntity2DetailedVo(entity);
        log.debug("DeliveryVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<DeliveryVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalAccountId, Optional<String> optionalSequence) throws MeetingException {
        if(optionalAccountId.isEmpty() && optionalSequence.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String accountId = optionalAccountId.orElse("");
        String sequence = optionalSequence.orElse("");
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(accountId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(sequence))) {
            log.debug("All search parameters are empty");
        }
        List<DeliveryVo> matchedDeliveryList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        DeliveryEntity entity = new DeliveryEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll().withIgnorePaths("id").withIgnoreNullValues();
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
        Example<DeliveryEntity> deliveryEntityExample = Example.of(entity, matcherCriteria);
        List<DeliveryEntity> deliveryEntityList = this.getMeetingRepository().findAll(deliveryEntityExample);
        matchedDeliveryList = encounterServiceHelper.deliveryEntity2DetailedVo(deliveryEntityList);

        if(matchedDeliveryList.isEmpty()){
            log.info("Found {} DeliveryVo matching with provided parameters : {}", matchedDeliveryList.size(), providedFilters);
        }else{
            log.info("No DeliveryVo available matching with provided parameters : {}", providedFilters);
        }

        return matchedDeliveryList;
    }

    @Override
    public DeliveryVo retrieveMatchingDetailsBySequenceOnDate(String sequence, String date) throws MeetingException {
        LocalDate dt = LocalDate.now();
        LocalDateTime start = LocalDateTime.now();
        LocalDateTime end = LocalDateTime.now();
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(deliveryTimeFormat);

        try {
            dt = LocalDate.parse(date, dtf);
        } catch (DateTimeParseException e) {
            log.debug("Date: {} format is invalid", date);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "date", date });
        }

        start = LocalDateTime.of(dt, LocalTime.of(0,0, 0));
        end = LocalDateTime.of(dt, LocalTime.of(23,59, 59));

        log.info("Requesting DeliveryEntity by sequence: {} between timestamps: {} and {}", sequence, start, end);
        Optional<DeliveryEntity> optEntity = this.getMeetingRepository().findBySequenceAndCreatedOnBetween(sequence, start, end);
        if(optEntity.isEmpty()) {
            log.debug("No DeliveryEntity found by sequence: {} between timestamps: {} and {}", sequence, start, end);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "sequence: " + sequence, ", date: " + date });
        }
        log.info("Found DeliveryVo by sequence: {} between timestamps: {} and {}", sequence, start, end);
        DeliveryEntity entity = optEntity.get();
        return encounterServiceHelper.deliveryEntity2DetailedVo(entity);
    }

    @Override
    public String createMeeting(DeliveryForm form) throws MeetingException {
        log.info("Creating new DeliveryEntity");

        if(form == null) {
            log.debug("DeliveryForm provided is null");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of DeliveryForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        this.getMeetingFormValidator().validate(form, err);
        if(err.hasErrors()) {
            log.debug("DeliveryForm has {} errors", err.getErrorCount());
            EncounterErrorCode ec = EncounterErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("DeliveryForm error detail: {}", ec);
            throw new MeetingException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of DeliveryForm are valid");

        DeliveryEntity expectedEntity = this.getMeetingForm2EntityConverter().convert(form);

        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), form.getAccountId(), form.getSequence());
        if(this.getMeetingRepository().existsByAccountIdAndSequence(expectedEntity.getAccountId(), expectedEntity.getSequence())) {
            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), expectedEntity.getAccountId(), expectedEntity.getSequence());
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_EXISTS, new Object[]{ "accountId: " + form.getAccountId(), "sequence: " + form.getSequence() });
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE.getValue(), expectedEntity.getAccountId(), expectedEntity.getSequence());

        log.debug("Saving {}", expectedEntity);
        DeliveryEntity actualEntity = this.getMeetingRepository().save(expectedEntity);
        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist DeliveryForm details" });
        }
        log.debug("Saved {}", actualEntity);
        log.info("Created new DeliveryForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Override
    public void updateMeeting(String id, DeliveryForm form) throws MeetingException {
        log.info("Updating DeliveryForm by id: {}", id);

        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_MEETING_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<DeliveryEntity> optActualEntity = this.getMeetingRepository().findById(idL);
        if(optActualEntity.isEmpty()) {
            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_NO_MEETING_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_FOUND_MEETING_ENTITY_ID.getValue(), id);

        DeliveryEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("DeliveryEntity is inactive with id: {}", id);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("DeliveryEntity is active with id: {}", id);

        if(form == null) {
            log.debug("DeliveryForm is null");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of DeliveryForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = this.getMeetingFormRelaxedValidator().validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("DeliveryForm has {} errors", err.getErrorCount());
            EncounterErrorCode ec = EncounterErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("DeliveryForm error detail: {}", ec);
            throw new MeetingException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of DeliveryForm are empty");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of DeliveryForm are valid");

        Optional<DeliveryEntity> optExpectedEntity = this.getMeetingForm2EntityMapper().compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of DeliveryForm");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from DeliveryForm to DeliveryEntity");

        DeliveryEntity expectedEntity = optExpectedEntity.get();

        this.checkUniquenessOfMeeting(form, actualEntity);

        this.getMeetingEntitySelfMapper().compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from DeliveryEntity to DeliveryForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = (DeliveryEntity) this.getMeetingRepository().save(actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency checkIn details" });
        }
        log.debug("Updated: {}", actualEntity);
        log.info("Updated existing DeliveryEntity with id: {}", actualEntity.getId());
    }

    @Override
    public void deleteMeeting(String id) throws MeetingException {
        log.info("Soft deleting DeliveryEntity by id: {}", id);

        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_MEETING_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<DeliveryEntity> optEntity = this.getMeetingRepository().findById(idL);
        if(optEntity.isEmpty()) {
            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_NO_MEETING_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_FOUND_MEETING_ENTITY_ID.getValue(), id);

        DeliveryEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("DeliveryEntity is inactive with id: {}", id);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("DeliveryEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        DeliveryEntity expectedEntity = (DeliveryEntity) this.getMeetingRepository().save(actualEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current checkIn details with id:" + id });
        }
        log.debug("Soft deleted: {}", expectedEntity);
        log.info("Soft deleted existing DeliveryEntity with id: {}", actualEntity.getId());
    }

    @Override
    public void applyPatchOnMeeting(String id, List<PatchOperationForm> patches) throws MeetingException {
        log.info("Patching DeliveryEntity by id: {}", id);

        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_MEETING_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<DeliveryEntity> optActualEntity = this.getMeetingRepository().findById(idL);
        if(optActualEntity.isEmpty()) {
            log.debug(MeetingMessageTemplate.MSG_TEMPLATE_NO_MEETING_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_FOUND_MEETING_ENTITY_ID.getValue(), id);

        DeliveryEntity actualEntity = optActualEntity.get();
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


        log.debug("Patching list items to DeliveryDto");
        DeliveryDto patchedDeliveryForm = new DeliveryDto();
        try {
            log.debug("Preparing patch list items for Meeting");
            JsonNode deliveryDtoTree = OBJECT_MAPPER.convertValue(patches, JsonNode.class);
            JsonPatch checkInPatch = JsonPatch.fromJson(deliveryDtoTree);
            log.debug("Prepared patch list items for Meeting");
            JsonNode blankDeliveryDtoTree = OBJECT_MAPPER.convertValue(new DeliveryDto(), JsonNode.class);
            JsonNode patchedDeliveryFormTree = checkInPatch.apply(blankDeliveryDtoTree);
            log.debug("Applying patch list items to DeliveryDto");
            patchedDeliveryForm = OBJECT_MAPPER.treeToValue(patchedDeliveryFormTree, DeliveryDto.class);
            log.debug("Applied patch list items to DeliveryDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to DeliveryDto: {}", e);
            MeetingException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in DeliveryDto");
                ex = new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to DeliveryDto: {}", e);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to DeliveryDto");

        log.debug("Validating patched DeliveryDto");
        Errors err = new DirectFieldBindingResult(patchedDeliveryForm, patchedDeliveryForm.getClass().getSimpleName());
        this.getMeetingDtoValidator().validate(patchedDeliveryForm, err);
        if(err.hasErrors()) {
            log.debug("Patched DeliveryDto has {} errors", err.getErrorCount());
            EncounterErrorCode ec = EncounterErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched DeliveryDto error detail: {}", ec);
            throw new MeetingException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched DeliveryDto are valid");

        this.checkUniquenessOfMeeting(patchedDeliveryForm, actualEntity);

        log.debug("Comparatively copying patched attributes from DeliveryDto to DeliveryEntity");
        try {
            this.getMeetingDto2EntityConverter().compareAndMapChild(patchedDeliveryForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (MeetingException) e;
        }
        log.debug("Comparatively copied patched attributes from DeliveryDto to DeliveryEntity");

        log.debug("Saving patched DeliveryEntity: {}", actualEntity);
        actualEntity = (DeliveryEntity) this.getMeetingRepository().save(actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete DeliveryEntity with id:{}", id);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency checkIn details with id:" + id });
        }
        log.debug("Saved patched DeliveryEntity: {}", actualEntity);
        log.info("Patched DeliveryEntity with id:{}", id);
    }
}