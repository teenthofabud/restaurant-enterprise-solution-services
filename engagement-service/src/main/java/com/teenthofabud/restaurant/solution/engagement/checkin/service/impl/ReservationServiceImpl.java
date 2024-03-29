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
import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.ReservationDto2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.ReservationEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.ReservationForm2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInMessageTemplate;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationDto;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.factory.CheckInBeanFactory;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.ReservationEntitySelfMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.ReservationForm2EntityMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.ReservationRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.ReservationService;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.ReservationDtoValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.ReservationFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.ReservationFormValidator;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import com.teenthofabud.restaurant.solution.engagement.utils.EngagementServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.data.jpa.domain.Specification;
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
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

@Slf4j
public class ReservationServiceImpl implements ReservationService {
    private String reservationTimeFormat;
    private String reservationDateFormat;
    private CheckInBeanFactory checkInBeanFactory;
    private EngagementServiceHelper engagementServiceHelper;
    private TOABBaseService toabBaseService;
    private ObjectMapper objectMapper;

    @Override
    @Value("${res.engagement.checkIn.reservation.time.format}")
    public void setReservationTimeFormat(String reservationTimeFormat) {
        this.reservationTimeFormat = reservationTimeFormat;
    }

    @Override
    @Value("${res.engagement.checkIn.reservation.date.format}")
    public void setReservationDateFormat(String reservationDateFormat) {
        this.reservationDateFormat = reservationDateFormat;
    }

    @Autowired
    public void setObjectMapper(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setCheckInBeanFactory(CheckInBeanFactory checkInBeanFactory) {
        this.checkInBeanFactory = checkInBeanFactory;
    }

    @Override
    public String getContextCheckInType() {
        return CheckInType.RESERVATION.name();
    }

    @Autowired
    public void setEngagementServiceHelper(EngagementServiceHelper engagementServiceHelper) {
        this.engagementServiceHelper = engagementServiceHelper;
    }

    @Override
    public ReservationFormValidator getCheckInFormValidator() {
        return (ReservationFormValidator) checkInBeanFactory.getCheckInFormValidator(getContextCheckInType()).get();
    }

    @Override
    public ReservationFormRelaxedValidator getCheckInFormRelaxedValidator() {
        return (ReservationFormRelaxedValidator) checkInBeanFactory.getCheckInFormRelaxedValidator(getContextCheckInType()).get();
    }

    @Override
    public ReservationDtoValidator getCheckInDtoValidator() {
        return (ReservationDtoValidator) checkInBeanFactory.getCheckInDtoValidator(getContextCheckInType()).get();
    }

    @Override
    public ReservationRepository getCheckInRepository() {
        return (ReservationRepository) checkInBeanFactory.getCheckInRepository(getContextCheckInType()).get();
    }

    @Override
    public ReservationEntitySelfMapper getCheckInEntitySelfMapper() {
        return (ReservationEntitySelfMapper) checkInBeanFactory.getCheckInEntitySelfMapper(getContextCheckInType()).get();
    }

    @Override
    public ReservationForm2EntityMapper getCheckInForm2EntityMapper() {
        return (ReservationForm2EntityMapper) checkInBeanFactory.getCheckInForm2EntityMapper(getContextCheckInType()).get();
    }

    @Override
    public ReservationForm2EntityConverter getCheckInForm2EntityConverter() {
        return (ReservationForm2EntityConverter) checkInBeanFactory.getCheckInForm2EntityConverter(getContextCheckInType()).get();
    }

    @Override
    public ReservationEntity2VoConverter getCheckInEntity2VoConverter() {
        return (ReservationEntity2VoConverter) checkInBeanFactory.getCheckInEntity2VoConverter(getContextCheckInType()).get();
    }

    @Override
    public ReservationDto2EntityConverter getCheckInDto2EntityConverter() {
        return (ReservationDto2EntityConverter) checkInBeanFactory.getCheckInDto2EntityConverter(getContextCheckInType()).get();
    }
    @Override
    public Comparator<ReservationVo> getCheckInVoTypeComparator() {
        return new Comparator<ReservationVo>() {
            @Override
            public int compare(ReservationVo s1, ReservationVo s2) {
                int c1 = s1.getAccountId().compareTo(s2.getAccountId());
                if (c1 == 0) {
                    int c2 = s1.getSequence().compareTo(s2.getSequence());
                    if (c2 == 0) {
                        /*int c3 = s1.getPhoneNumber().compareTo(s2.getPhoneNumber());
                        return c3;*/
                        int c3 = s1.getDate().compareTo(s2.getDate());
                        if(c3 == 0) {
                            return s1.getTime().compareTo(s2.getTime());
                        } else {
                            return c3;
                        }
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
    public List<ReservationVo> retrieveAllMatchingReservationDetailsByCriteria(Optional<String> optionalDate, Optional<String> optionalTime) throws CheckInException {
        if(!optionalDate.isPresent() && !optionalTime.isPresent()) {
            log.debug("No search parameters provided");
        }
        String date = optionalDate.isPresent() ? optionalDate.get() : "";
        String time = optionalTime.isPresent() ? optionalTime.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(date)) && StringUtils.isEmpty(StringUtils.trimWhitespace(time))) {
            log.debug("All search parameters are empty");
        }
        List<ReservationVo> matchedReservationList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        List<Specification<ReservationEntity>> reservationEntitySpecificationCollection = new ArrayList<>(2);
        if(StringUtils.hasText(StringUtils.trimWhitespace(date))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(reservationDateFormat);
                LocalDate dt = LocalDate.parse(date, dtf);
                log.debug("date {} is valid", date);
                providedFilters.put("date", date);
                Specification<ReservationEntity> dateEquals = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("date"), dt);
                reservationEntitySpecificationCollection.add(dateEquals);
            } catch (DateTimeParseException e) {
                log.error("Unable to parse date", e);
                log.debug("Reservation date: {} is invalid", date);
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "date", date });
            }
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(time))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(reservationTimeFormat);
                LocalTime t = LocalTime.parse(time, dtf);
                log.debug("time {} is valid", time);
                LocalTime start = LocalTime.of(t.getHour(), t.getMinute(), 0);
                LocalTime end = LocalTime.of(t.getHour(), t.getMinute(), 59);
                providedFilters.put("time:start", start.format(DateTimeFormatter.ofPattern(reservationTimeFormat)));
                providedFilters.put("time:end", end.format(DateTimeFormatter.ofPattern(reservationTimeFormat)));
                Specification<ReservationEntity> timeBetween = (root, query, criteriaBuilder) -> criteriaBuilder.between(root.get("time"), start, end);
                reservationEntitySpecificationCollection.add(timeBetween);
            } catch (DateTimeParseException e) {
                log.error("Unable to parse time", e);
                log.debug("Reservation time: {} is invalid", time);
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "time", time });
            }
        }
        Specification<ReservationEntity> reservationEntitySpecification = null;
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            for(int  i = 0 ; i < reservationEntitySpecificationCollection.size() ; i++) {
                Specification<ReservationEntity> entitySpecification = reservationEntitySpecificationCollection.get(i);
                if(i == 0) {
                    reservationEntitySpecification = Specification.where(entitySpecification);
                } else {
                    reservationEntitySpecification = reservationEntitySpecification.and(entitySpecification);
                }
            }
            log.debug("search parameters {} are valid", providedFilters);
            List<ReservationEntity> reservationEntityList = this.getCheckInRepository().findAll(reservationEntitySpecification);
            matchedReservationList = this.engagementServiceHelper.reservationEntity2DetailedVo(reservationEntityList);
            Collections.sort(matchedReservationList, getCheckInVoTypeComparator());
        }
        log.info("Found {} ReservationVo matching with provided parameters : {}", matchedReservationList.size(), providedFilters);
        log.info("No ReservationVo available matching with provided parameters : {}", matchedReservationList.size(), providedFilters);
        return matchedReservationList;
    }

    @Override
    public Set<ReservationVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all ReservationEntity by their natural ordering");
        List<ReservationEntity> reservationEntityList = this.getCheckInRepository().findAll();
        List<ReservationVo> reservationVoList = engagementServiceHelper.reservationEntity2DetailedVo(reservationEntityList);
        Set<ReservationVo> naturallyOrderedSet = new TreeSet<>(getCheckInVoTypeComparator());
        naturallyOrderedSet.addAll(reservationVoList);
        log.info("{} ReservationVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public ReservationVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws CheckInException {
        log.info("Requesting ReservationEntity by id: {}", id);
        Long idL = this.parsePK(id);
        ReservationRepository reservationRepository = this.getCheckInRepository();
        Optional<ReservationEntity> optEntity = reservationRepository.findById(idL);
        if(!optEntity.isPresent()) {
            log.debug("No ReservationEntity found by id: {}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found ReservationVo by id: {}", id);
        ReservationEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        ReservationVo vo = engagementServiceHelper.reservationEntity2DetailedVo(entity);
        log.debug("ReservationVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<ReservationVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalAccountId, Optional<String> optionalSequence, Optional<String> optionalNotes) throws CheckInException {
        if(!optionalAccountId.isPresent() && !optionalSequence.isPresent() && !optionalNotes.isPresent()) {
            log.debug("No search parameters provided");
        }
        String accountId = optionalAccountId.isPresent() ? optionalAccountId.get() : "";
        String sequence = optionalSequence.isPresent() ? optionalSequence.get() : "";
        String notes = optionalNotes.isPresent() ? optionalNotes.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(accountId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(sequence)) && StringUtils.isEmpty(StringUtils.trimWhitespace(notes))) {
            log.debug("All search parameters are empty");
        }
        List<ReservationVo> matchedReservationList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        ReservationEntity entity = new ReservationEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll().withIgnoreNullValues();
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
        if(StringUtils.hasText(StringUtils.trimWhitespace(notes))) {
            log.debug("notes {} is valid", notes);
            providedFilters.put("notes", notes);
            entity.setNotes(notes);
            matcherCriteria = matcherCriteria.withMatcher("notes", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<ReservationEntity> reservationEntityExample = Example.of(entity, matcherCriteria);
        List<ReservationEntity> reservationEntityList = this.getCheckInRepository().findAll(reservationEntityExample);
        matchedReservationList = engagementServiceHelper.reservationEntity2DetailedVo(reservationEntityList);
        Collections.sort(matchedReservationList, getCheckInVoTypeComparator());
        log.info("Found {} ReservationVo matching with provided parameters : {}", matchedReservationList.size(), providedFilters);
        log.info("No ReservationVo available matching with provided parameters : {}", matchedReservationList.size(), providedFilters);
        return matchedReservationList;
    }

    @Override
    public ReservationVo retrieveMatchingDetailsByCriteria(String sequence, String date) throws CheckInException {
        LocalDate dt = LocalDate.now();
        LocalDateTime start = LocalDateTime.now();
        LocalDateTime end = LocalDateTime.now();
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(reservationDateFormat);

        try {
            dt = LocalDate.parse(date, dtf);
        } catch (DateTimeParseException e) {
            log.debug("Date: {} format is invalid", date);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "date", date });
        }

        start = LocalDateTime.of(dt, LocalTime.of(0,0, 0));
        end = LocalDateTime.of(dt, LocalTime.of(23,59, 59));

        log.info("Requesting ReservationEntity by sequence: {} between timestamps: {} and {}", sequence, start, end);
        Optional<ReservationEntity> optEntity = this.getCheckInRepository().findBySequenceAndCreatedOnBetween(sequence, start, end);
        if(!optEntity.isPresent()) {
            log.debug("No ReservationEntity found by sequence: {} between timestamps: {} and {}", sequence, start, end);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "sequence: " + sequence, ", date: " + date });
        }
        log.info("Found ReservationVo by sequence: {} between timestamps: {} and {}", sequence, start, end);
        ReservationEntity entity = optEntity.get();
        ReservationVo vo = engagementServiceHelper.reservationEntity2DetailedVo(entity);
        return vo;
    }

    @Override
    public String createCheckIn(ReservationForm form) throws CheckInException {
        log.info("Creating new ReservationEntity");

        if(form == null) {
            log.debug("ReservationForm provided is null");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of ReservationForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        this.getCheckInFormValidator().validate(form, err);
        if(err.hasErrors()) {
            log.debug("ReservationForm has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("ReservationForm error detail: {}", ec);
            throw new CheckInException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of ReservationForm are valid");

        ReservationEntity expectedEntity = this.getCheckInForm2EntityConverter().convert(form);

        LocalDate dt = LocalDate.now();
        LocalDateTime start = LocalDateTime.of(dt, LocalTime.of(0,0, 0));
        LocalDateTime end = LocalDateTime.of(dt, LocalTime.of(23,59, 59));

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(), form.getAccountId(), form.getSequence(), start, end);
        if(this.getCheckInRepository().existsByAccountIdAndSequenceAndCreatedOnBetween(expectedEntity.getAccountId(), expectedEntity.getSequence(), start, end)) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_EXISTS_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(),
                    expectedEntity.getAccountId(), expectedEntity.getSequence(), start, end);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_EXISTS, new Object[]{ "accountId: " + form.getAccountId(), "sequence: " + form.getSequence() + ", start: " + start + ", end: " + end });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_NON_EXISTENCE_BY_ACCOUNT_ID_AND_SEQUENCE_AND_CREATED_BETWEEN.getValue(), expectedEntity.getAccountId(), expectedEntity.getSequence(), start, end);

        log.debug("Saving {}", expectedEntity);
        ReservationEntity actualEntity = this.getCheckInRepository().save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist ReservationForm details" });
        }
        log.info("Created new ReservationForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Override
    public void updateCheckIn(String id, ReservationForm form) throws CheckInException {
        log.info("Updating ReservationForm by id: {}", id);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<ReservationEntity> optActualEntity = this.getCheckInRepository().findById(idL);
        if(!optActualEntity.isPresent()) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        ReservationEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("ReservationEntity is inactive with id: {}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("ReservationEntity is active with id: {}", id);

        if(form == null) {
            log.debug("ReservationForm is null");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of ReservationForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = this.getCheckInFormRelaxedValidator().validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("ReservationForm has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("ReservationForm error detail: {}", ec);
            throw new CheckInException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of ReservationForm are empty");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of ReservationForm are valid");

        Optional<ReservationEntity> optExpectedEntity = this.getCheckInForm2EntityMapper().compareAndMap(actualEntity, form);
        if(!optExpectedEntity.isPresent()) {
            log.debug("No new value for attributes of ReservationForm");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from ReservationForm to ReservationEntity");

        ReservationEntity expectedEntity = optExpectedEntity.get();

        this.checkUniquenessOfCheckIn(form, actualEntity);

        this.getCheckInEntitySelfMapper().compareAndMapChild(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from ReservationEntity to ReservationForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = (ReservationEntity) this.getCheckInRepository().save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency checkIn details" });
        }
        log.info("Updated existing ReservationEntity with id: {}", actualEntity.getId());
    }

    @Override
    public void deleteCheckIn(String id) throws CheckInException {
        log.info("Soft deleting ReservationEntity by id: {}", id);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<ReservationEntity> optEntity = this.getCheckInRepository().findById(idL);
        if(!optEntity.isPresent()) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        ReservationEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("ReservationEntity is inactive with id: {}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("ReservationEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        ReservationEntity expectedEntity = (ReservationEntity) this.getCheckInRepository().save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current checkIn details with id:" + id });
        }

        log.info("Soft deleted existing ReservationEntity with id: {}", actualEntity.getId());

    }

    @Override
    public void applyPatchOnCheckIn(String id, List<PatchOperationForm> patches) throws CheckInException {
        log.info("Patching ReservationEntity by id: {}", id);

        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CHECKIN_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<ReservationEntity> optActualEntity = this.getCheckInRepository().findById(idL);
        if(!optActualEntity.isPresent()) {
            log.debug(CheckInMessageTemplate.MSG_TEMPLATE_NO_CHECKIN_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_FOUND_CHECKIN_ENTITY_ID.getValue(), id);

        ReservationEntity actualEntity = optActualEntity.get();
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


        log.debug("Patching list items to ReservationDto");
        ReservationDto patchedReservationDto = new ReservationDto();
        try {
            log.debug("Preparing patch list items for CheckIn");
            JsonNode reservationDtoTree = objectMapper.convertValue(patches, JsonNode.class);
            JsonPatch checkInPatch = JsonPatch.fromJson(reservationDtoTree);
            log.debug("Prepared patch list items for CheckIn");
            JsonNode blankReservationDtoTree = objectMapper.convertValue(new ReservationDto(), JsonNode.class);
            JsonNode patchedReservationFormTree = checkInPatch.apply(blankReservationDtoTree);
            log.debug("Applying patch list items to ReservationDto");
            patchedReservationDto = objectMapper.treeToValue(patchedReservationFormTree, ReservationDto.class);
            log.debug("Applied patch list items to ReservationDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to ReservationDto: {}", e);
            CheckInException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in ReservationDto");
                ex = new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to ReservationDto: {}", e);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to ReservationDto");

        log.debug("Validating patched ReservationDto");
        Errors err = new DirectFieldBindingResult(patchedReservationDto, patchedReservationDto.getClass().getSimpleName());
        this.getCheckInDtoValidator().validate(patchedReservationDto, err);
        if(err.hasErrors()) {
            log.debug("Patched ReservationDto has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched ReservationDto error detail: {}", ec);
            throw new CheckInException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched ReservationDto are valid");

        this.checkUniquenessOfCheckIn(patchedReservationDto, actualEntity);

        log.debug("Comparatively copying patched attributes from ReservationDto to ReservationEntity");
        try {
            this.getCheckInDto2EntityConverter().compareAndMapChild(patchedReservationDto, actualEntity);
        } catch (TOABBaseException e) {
            throw (CheckInException) e;
        }
        log.debug("Comparatively copied patched attributes from ReservationDto to ReservationEntity");

        log.debug("Saving patched ReservationEntity: {}", actualEntity);
        actualEntity = this.getCheckInRepository().save(actualEntity);
        log.debug("Saved patched ReservationEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete ReservationEntity with id:{}", id);
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency checkIn details with id:" + id });
        }
        log.info("Patched ReservationEntity with id:{}", id);
    }
}