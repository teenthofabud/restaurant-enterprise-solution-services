package com.teenthofabud.restaurant.solution.encounter.meeting.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingDto2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.meeting.converter.MeetingForm2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.*;
import com.teenthofabud.restaurant.solution.encounter.meeting.mapper.MeetingEntitySelfMapper;
import com.teenthofabud.restaurant.solution.encounter.meeting.mapper.MeetingForm2EntityMapper;
import com.teenthofabud.restaurant.solution.encounter.meeting.repository.MeetingRepository;
import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingDtoValidator;
import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.encounter.meeting.validator.MeetingFormValidator;
import constants.EncounterErrorCode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.*;

@Service
public interface MeetingService<T extends MeetingForm, V extends MeetingVo,
        A extends MeetingFormValidator, B extends MeetingFormRelaxedValidator, C extends MeetingDtoValidator,
        D extends MeetingRepository, E extends MeetingEntitySelfMapper, F extends MeetingForm2EntityMapper,
        G extends MeetingForm2EntityConverter, H extends MeetingEntity2VoConverter, I extends MeetingDto2EntityConverter> {

    static final ObjectMapper OBJECT_MAPPER = new ObjectMapper().registerModule(new Jdk8Module());

    static Logger LOGGER = LoggerFactory.getLogger(MeetingService.class);

    public String getContextMeetingType();

    public A getMeetingFormValidator();

    public B getMeetingFormRelaxedValidator();

    public C getMeetingDtoValidator();

    public D getMeetingRepository();

    public E getMeetingEntitySelfMapper();

    public F getMeetingForm2EntityMapper();

    public G getMeetingForm2EntityConverter();

    public H getMeetingEntity2VoConverter();

    public I getMeetingDto2EntityConverter();

    public Comparator<V> getMeetingVoTypeComparator();

    public Set<V> retrieveAllByNaturalOrdering();

    public V retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws MeetingException;

    public List<V> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalAccountId,
                                                        Optional<String> optionalSequence) throws MeetingException;

    public V retrieveMatchingDetailsBySequenceOnDate(String sequence, String date) throws MeetingException;
    
    public String createMeeting(T form) throws MeetingException;

    public void updateMeeting(String id, T form) throws MeetingException;

    public void deleteMeeting(String id) throws MeetingException;

    public void applyPatchOnMeeting(String id, List<PatchOperationForm> patches) throws MeetingException;

    default Long parsePK(String id) throws MeetingException {
        Long checkInId = -1L;
        try {
            checkInId = Long.parseLong(id);
            if(checkInId <= 0) {
                LOGGER.debug("PK {} can't be negative", id);
                throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        } catch (NumberFormatException e) {
            LOGGER.error("Unable to parse PK: {}", id, e);
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return checkInId;
    }

    default void checkUniquenessOfMeeting(MeetingDto patchedReservationForm, MeetingEntity actualEntity) throws MeetingException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(2);
        if(patchedReservationForm.getAccountId().isPresent()) {
            similaritySwitchesCollection.add(patchedReservationForm.getAccountId().get().compareTo(actualEntity.getAccountId()) == 0);
        }
        if(patchedReservationForm.getSequence().isPresent()) {
            similaritySwitchesCollection.add(patchedReservationForm.getSequence().get().compareTo(actualEntity.getSequence()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String accountId = patchedReservationForm.getAccountId().isPresent() ? patchedReservationForm.getAccountId().get() : actualEntity.getAccountId();
            String sequence = patchedReservationForm.getSequence().isPresent() ? patchedReservationForm.getSequence().get() : actualEntity.getSequence();
            LocalDate dt = LocalDate.now();
            LocalDateTime start = LocalDateTime.of(dt, LocalTime.of(0,0, 0));
            LocalDateTime end = LocalDateTime.of(dt, LocalTime.of(23,59, 59));
            LOGGER.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(), accountId, sequence, start, end);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  this.getMeetingRepository().existsByAccountIdAndSequenceAndCreatedOnBetween(accountId, sequence, start, end);
            if(sameEntitySw || duplicateEntitySw) {
                LOGGER.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(), accountId, sequence, start, end);
                throw new MeetingException(EncounterErrorCode.ENCOUNTER_EXISTS, new Object[]{ "accountId: " + accountId, ", sequence: " + sequence + ", start: " + start + ", end: " + end });
            }
            LOGGER.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(), accountId, sequence, start, end);

        }
    }

    default void checkUniquenessOfMeeting(MeetingForm reservationForm, MeetingEntity actualEntity) throws MeetingException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(3);
        if(StringUtils.hasText(StringUtils.trimWhitespace(reservationForm.getAccountId()))) {
            similaritySwitchesCollection.add(reservationForm.getAccountId().compareTo(actualEntity.getAccountId()) == 0);
        }
        if(!ObjectUtils.isEmpty(reservationForm.getSequence())) {
            similaritySwitchesCollection.add(reservationForm.getSequence().compareTo(actualEntity.getSequence()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String accountId = StringUtils.hasText(StringUtils.trimWhitespace(reservationForm.getAccountId())) ? reservationForm.getAccountId() : actualEntity.getAccountId();
            String sequence = !ObjectUtils.isEmpty(reservationForm.getSequence()) ? reservationForm.getSequence() : actualEntity.getSequence();
            LocalDate dt = LocalDate.now();
            LocalDateTime start = LocalDateTime.of(dt, LocalTime.of(0,0, 0));
            LocalDateTime end = LocalDateTime.of(dt, LocalTime.of(23,59, 59));
            LOGGER.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(), accountId, sequence, start, end);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  this.getMeetingRepository().existsByAccountIdAndSequenceAndCreatedOnBetween(accountId, sequence, start, end);
            if(sameEntitySw || duplicateEntitySw) {
                LOGGER.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(), accountId, sequence, start, end);
                throw new MeetingException(EncounterErrorCode.ENCOUNTER_EXISTS, new Object[]{ "accountId: " + accountId, ", sequence: " + sequence + ", start: " + start + ", end: " + end });
            }
            LOGGER.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(), accountId, sequence, start, end);
        }
    }
}
