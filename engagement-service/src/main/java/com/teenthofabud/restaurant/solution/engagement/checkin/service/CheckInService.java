package com.teenthofabud.restaurant.solution.engagement.checkin.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.factory.CheckInBeanFactory;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.CheckInEntitySelfMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.CheckInForm2EntityMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.CheckInRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.*;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import com.teenthofabud.restaurant.solution.engagement.utils.EngagementServiceHelper;
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
public interface CheckInService<T extends CheckInForm, V extends CheckInVo,
        A extends CheckInFormValidator, B extends CheckInFormRelaxedValidator, C extends CheckInDtoValidator,
        D extends CheckInRepository, E extends CheckInEntitySelfMapper, F extends CheckInForm2EntityMapper,
        G extends CheckInForm2EntityConverter, H extends CheckInEntity2VoConverter, I extends CheckInDto2EntityConverter> {

    static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    static Logger LOGGER = LoggerFactory.getLogger(CheckInService.class);

    public String getContextCheckInType();

    public A getCheckInFormValidator();

    public B getCheckInFormRelaxedValidator();

    public C getCheckInDtoValidator();

    public D getCheckInRepository();

    public E getCheckInEntitySelfMapper();

    public F getCheckInForm2EntityMapper();

    public G getCheckInForm2EntityConverter();

    public H getCheckInEntity2VoConverter();

    public I getCheckInDto2EntityConverter();

    default Long parsePK(String id) throws CheckInException {
        Long checkInId = -1L;
        try {
            checkInId = Long.parseLong(id);
            if(checkInId <= 0) {
                LOGGER.debug("PK {} can't be negative", id);
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        } catch (NumberFormatException e) {
            LOGGER.error("Unable to parse PK: {}", id, e);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return checkInId;
    }

    default void checkUniquenessOfCheckIn(CheckInDto patchedReservationForm, CheckInEntity actualEntity) throws CheckInException {
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
            LOGGER.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(), accountId, sequence, start, end);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  this.getCheckInRepository().existsByAccountIdAndSequenceAndCreatedOnBetween(accountId, sequence, start, end);
            if(sameEntitySw || duplicateEntitySw) {
                LOGGER.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(), accountId, sequence, start, end);
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_EXISTS, new Object[]{ "accountId: " + accountId, ", sequence: " + sequence + ", start: " + start + ", end: " + end });
            }
            LOGGER.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(), accountId, sequence, start, end);

        }
    }

    default void checkUniquenessOfCheckIn(CheckInForm reservationForm, CheckInEntity actualEntity) throws CheckInException {
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
            LOGGER.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(), accountId, sequence, start, end);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  this.getCheckInRepository().existsByAccountIdAndSequenceAndCreatedOnBetween(accountId, sequence, start, end);
            if(sameEntitySw || duplicateEntitySw) {
                LOGGER.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(), accountId, sequence, start, end);
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_EXISTS, new Object[]{ "accountId: " + accountId, ", sequence: " + sequence + ", start: " + start + ", end: " + end });
            }
            LOGGER.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(), accountId, sequence, start, end);
        }
    }

    public Comparator<V> getCheckInVoTypeComparator();

    public Set<V> retrieveAllByNaturalOrdering();

    public V retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws CheckInException;

    public List<V> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalAccountId,
                                                                Optional<String> optionalSequence,
                                                                Optional<String> optionalNotes) throws CheckInException;

    public V retrieveMatchingDetailsByCriteria(String sequence, String date) throws CheckInException;

    public String createCheckIn(T form) throws CheckInException;

    public void updateCheckIn(String id, T form) throws CheckInException;

    public void deleteCheckIn(String id) throws CheckInException;

    public void applyPatchOnCheckIn(String id, List<PatchOperationForm> patches) throws CheckInException;

}
