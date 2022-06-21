package com.teenthofabud.restaurant.solution.reservation.engagement.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.fge.jsonpatch.JsonPatch;
import com.github.fge.jsonpatch.JsonPatchException;
import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.restaurant.solution.reservation.engagement.converter.EngagementDto2DocumentConverter;
import com.teenthofabud.restaurant.solution.reservation.engagement.converter.EngagementForm2DocumentConverter;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.*;
import com.teenthofabud.restaurant.solution.reservation.engagement.mapper.EngagementDocumentSelfMapper;
import com.teenthofabud.restaurant.solution.reservation.engagement.mapper.EngagementForm2DocumentMapper;
import com.teenthofabud.restaurant.solution.reservation.engagement.repository.DeliveryEngagementRepository;
import com.teenthofabud.restaurant.solution.reservation.engagement.repository.DineInEngagementRepository;
import com.teenthofabud.restaurant.solution.reservation.engagement.repository.EngagementRepository;
import com.teenthofabud.restaurant.solution.reservation.engagement.repository.TakeAwayEngagementRepository;
import com.teenthofabud.restaurant.solution.reservation.engagement.service.EngagementService;
import com.teenthofabud.restaurant.solution.reservation.engagement.validator.EngagementDtoValidator;
import com.teenthofabud.restaurant.solution.reservation.engagement.validator.EngagementFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.reservation.engagement.validator.EngagementFormValidator;
import com.teenthofabud.restaurant.solution.reservation.error.ReservationErrorCode;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingException;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.reservation.booking.service.BookingService;
import com.teenthofabud.restaurant.solution.reservation.utils.ReservationServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.*;

@Component
@Slf4j
public class EngagementServiceImpl implements EngagementService {

    private static final Comparator<EngagementVo> CMP_BY_BOOKING_ID_TOKEN_NUMBER = (s1, s2) -> {
       int c1 = (s1.getBooking() == null || s2.getBooking() == null) ? s1.getBookingId().compareTo(s2.getBookingId())
               : s1.getBooking().compareTo(s2.getBooking());
       if(c1 == 0) {
           int c2 = s1.getTokenNumber().compareTo(s2.getTokenNumber());
           return c2;
       } else {
           return c1;
       }
    };

    private EngagementForm2DocumentConverter form2DocumentConverter;
    private EngagementDto2DocumentConverter dto2DocumentConverter;
    private EngagementForm2DocumentMapper form2DocumentMapper;
    private EngagementDocumentSelfMapper documentSelfMapper;
    private EngagementFormValidator formValidator;
    private EngagementFormRelaxedValidator relaxedFormValidator;
    private EngagementDtoValidator dtoValidator;
    //private EngagementRepository repository;
    private DeliveryEngagementRepository deliveryEngagementRepository;
    private TakeAwayEngagementRepository takeAwayEngagementRepository;
    private DineInEngagementRepository dineInEngagementRepository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private ReservationServiceHelper reservationServiceHelper;
    private BookingService bookingService;
    private EngagementRepository engagementRepository;

    @Autowired
    public void setEngagementRepository(EngagementRepository engagementRepository) {
        this.engagementRepository = engagementRepository;
    }

    @Autowired
    public void setBookingService(BookingService bookingService) {
        this.bookingService = bookingService;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }


    @Autowired
    public void setSessionServiceHelper(ReservationServiceHelper reservationServiceHelper) {
        this.reservationServiceHelper = reservationServiceHelper;
    }

    @Autowired
    public void setDto2DocumentConverter(EngagementDto2DocumentConverter dto2DocumentConverter) {
        this.dto2DocumentConverter = dto2DocumentConverter;
    }

    @Autowired
    public void setForm2DocumentMapper(EngagementForm2DocumentMapper form2DocumentMapper) {
        this.form2DocumentMapper = form2DocumentMapper;
    }

    @Autowired
    public void setDocumentSelfMapper(EngagementDocumentSelfMapper documentSelfMapper) {
        this.documentSelfMapper = documentSelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(EngagementFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchEngagementValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(EngagementDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2DocumentConverter(EngagementForm2DocumentConverter form2DocumentConverter) {
        this.form2DocumentConverter = form2DocumentConverter;
    }

    @Autowired
    public void setDeliveryEngagementRepository(DeliveryEngagementRepository deliveryEngagementRepository) {
        this.deliveryEngagementRepository = deliveryEngagementRepository;
    }

    @Autowired
    public void setTakeAwayEngagementRepository(TakeAwayEngagementRepository takeAwayEngagementRepository) {
        this.takeAwayEngagementRepository = takeAwayEngagementRepository;
    }

    public void setDineInEngagementRepository(DineInEngagementRepository dineInEngagementRepository) {
        this.dineInEngagementRepository = dineInEngagementRepository;
    }

    @Autowired
    public void setReservationServiceHelper(ReservationServiceHelper reservationServiceHelper) {
        this.reservationServiceHelper = reservationServiceHelper;
    }

    /*@Autowired
    public void setRepository(EngagementRepository repository) {
        this.repository = repository;
    }*/

    @Autowired
    public void setFormValidator(EngagementFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseEngagementId(String id) throws EngagementException {
        Long engagementId = null;
        try {
            engagementId = Long.parseLong(id);
            log.debug("Parsed id {} to engagement id {} in numeric format", id, engagementId);
            if(engagementId <= 0) {
                throw new NumberFormatException("engagement id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse engagement id", e);
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_ID_INVALID.getValue(), id);
            throw new EngagementException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return engagementId;
    }

    @Override
    public Set<EngagementVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all EngagementDocument by their natural ordering");
        List<? extends EngagementDocument> dineInEngagementDocumentList = dineInEngagementRepository.findAll();
        log.debug("{} DineInEngagementDocument available", dineInEngagementDocumentList.size());
        List<? extends EngagementDocument> takeAwayEngagementDocumentList = takeAwayEngagementRepository.findAll();
        log.debug("{} TakeAwayEngagementDocument available", takeAwayEngagementDocumentList.size());
        List<? extends EngagementDocument> deliveryEngagementDocumentList = deliveryEngagementRepository.findAll();
        log.debug("{} DeliveryEngagementDocument available", deliveryEngagementDocumentList.size());
        List<EngagementDocument> engagementDocumentList = new ArrayList<>();
        engagementDocumentList.addAll(dineInEngagementDocumentList);
        engagementDocumentList.addAll(takeAwayEngagementDocumentList);
        engagementDocumentList.addAll(deliveryEngagementDocumentList);
        List<EngagementVo> engagementVoList = reservationServiceHelper.engagementDocument2DetailedVo(engagementDocumentList);
        Set<EngagementVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_BOOKING_ID_TOKEN_NUMBER);
        naturallyOrderedSet.addAll(engagementVoList);
        log.info("{} EngagementVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public EngagementVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws EngagementException {
        log.info("Requesting EngagementDocument by id: {}", id);
        Optional<? extends EngagementDocument> optionalDineInEngagementDocument = dineInEngagementRepository.findById(id);
        Optional<? extends EngagementDocument> optionalTakeAwayEngagementDocument = takeAwayEngagementRepository.findById(id);
        Optional<? extends EngagementDocument> optionalDeliveryEngagementDocument = deliveryEngagementRepository.findById(id);
        if(optionalDineInEngagementDocument.isEmpty() && optionalTakeAwayEngagementDocument.isEmpty() && optionalDeliveryEngagementDocument.isEmpty()) {
            log.debug("No EngagementDocument found by id: {}", id);
            throw new EngagementException(ReservationErrorCode.RESERVATION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        EngagementDocument engagementDocument = optionalDineInEngagementDocument.isPresent() ? optionalDineInEngagementDocument.get() :
                optionalTakeAwayEngagementDocument.isPresent() ? optionalTakeAwayEngagementDocument.get() :
                optionalDeliveryEngagementDocument.get();
        log.info("Found EngagementVo by id: {}", id);
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        EngagementVo vo = reservationServiceHelper.engagementDocument2DetailedVo(engagementDocument);
        log.debug("EngagementVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<EngagementVo> retrieveAllMatchingDetailsByBookingId(String bookingId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws EngagementException {
        log.info("Requesting EngagementDocument that match with bookingId: {}", bookingId);
        try {
            BookingVo bookingVo = bookingService.retrieveDetailsById(bookingId, Optional.of(TOABCascadeLevel.ONE));
            if(!bookingVo.getActive()) {
                throw new BookingException(ReservationErrorCode.RESERVATION_INACTIVE, new Object [] { bookingId });
            }
        } catch (BookingException e) {
            log.error("bookingId is invalid", e);
            throw new EngagementException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object [] { "bookingId: " + bookingId });
        }
        List<? extends EngagementDocument> dineInEngagementDocumentList = dineInEngagementRepository.findByBookingId(bookingId);
        List<? extends EngagementDocument> deliveryEngagementDocumentList = deliveryEngagementRepository.findByBookingId(bookingId);
        List<? extends EngagementDocument> takeAwayEngagementDocumentList = takeAwayEngagementRepository.findByBookingId(bookingId);
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        List<EngagementDocument> engagementDocumentList = new ArrayList<>();
        if(dineInEngagementDocumentList.isEmpty() && deliveryEngagementDocumentList.isEmpty() && takeAwayEngagementDocumentList.isEmpty()) {
            log.debug("No EngagementDocument found by bookingId: {}", bookingId);
            throw new EngagementException(ReservationErrorCode.RESERVATION_NOT_FOUND, new Object[] { "bookingId", String.valueOf(bookingId) });
        }
        if(!dineInEngagementDocumentList.isEmpty() && !deliveryEngagementDocumentList.isEmpty() && !takeAwayEngagementDocumentList.isEmpty()) {
            log.debug("Multiple EngagementDocument found by bookingId: {}", bookingId);
            throw new EngagementException(ReservationErrorCode.RESERVATION_ACTION_FAILURE, new Object[] { "get by bookingId", "multiple matches  by type" });
        }
        engagementDocumentList.addAll(!dineInEngagementDocumentList.isEmpty() ? dineInEngagementDocumentList :
                !deliveryEngagementDocumentList.isEmpty() ? deliveryEngagementDocumentList : takeAwayEngagementDocumentList);
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        List<EngagementVo> matchedEngagementList = reservationServiceHelper.engagementDocument2DetailedVo(engagementDocumentList);
        log.info("Found {} EngagementVo matching with recipeId: {}", matchedEngagementList.size(), bookingId);
        TOABRequestContextHolder.clearCascadeLevelContext();
        if(matchedEngagementList.isEmpty()) {
            log.debug("No EngagementVo found matching with bookingId: {}", bookingId);
            throw new EngagementException(ReservationErrorCode.RESERVATION_NOT_FOUND, new Object[] { "bookingId", bookingId });
        }
        return matchedEngagementList;
    }

    @Override
    public List<EngagementVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalBookingId, Optional<String> optionalTokenNumber, Optional<String> optionalTableId, Optional<String> optionalExtRef, Optional<String> optionalInstructions) throws EngagementException {
        if(optionalBookingId.isEmpty() && optionalTableId.isEmpty() && optionalTokenNumber.isEmpty() && optionalExtRef.isEmpty() && optionalInstructions.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String bookingId = optionalBookingId.isPresent() ? optionalBookingId.get() : "";
        String tokenNumber = optionalTokenNumber.isPresent() ? optionalTokenNumber.get() : "";
        String tableId = optionalTableId.isPresent() ? optionalTableId.get() : "";
        String extRef = optionalExtRef.isPresent() ? optionalExtRef.get() : "";
        String instructions = optionalInstructions.isPresent() ? optionalInstructions.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(bookingId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(tokenNumber))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(extRef)) && StringUtils.isEmpty(StringUtils.trimWhitespace(tableId))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(instructions))) {
            log.debug("All search parameters are empty");
        }
        List<EngagementVo> matchedEngagementList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        EngagementDto engagementDto = new EngagementDto();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(bookingId))) {
            log.debug("bookingId {} is valid", bookingId);
            providedFilters.put("bookingId", bookingId);
            try {
                BookingVo bookingVo = bookingService.retrieveDetailsById(bookingId, Optional.of(TOABCascadeLevel.TWO));
                String categoryName = bookingVo.getCategory().getName();
                switch (categoryName) {
                    case "Dine In":
                        if(StringUtils.hasText(StringUtils.trimWhitespace(tableId))) {
                            log.debug("tableId {} is valid", tableId);
                            providedFilters.put("tableId", tableId);
                            engagementDto.setTableId(Optional.of(tableId));
                            matcherCriteria = matcherCriteria.withMatcher("tableId", match -> match.exact());
                        }
                        break;
                    case "Take Away":
                        if(StringUtils.hasText(StringUtils.trimWhitespace(instructions))) {
                            log.debug("instructions {} is valid", instructions);
                            providedFilters.put("instructions", instructions);
                            engagementDto.setInstructions(Optional.of(instructions));
                            matcherCriteria = matcherCriteria.withMatcher("instructions", match -> match.exact());
                        }
                        break;
                    case "Delivery":
                        if(StringUtils.hasText(StringUtils.trimWhitespace(extRef))) {
                            log.debug("extRef {} is valid", extRef);
                            providedFilters.put("extRef", extRef);
                            engagementDto.setExtRef(Optional.of(extRef));
                            matcherCriteria = matcherCriteria.withMatcher("extRef", match -> match.exact());
                        }
                        break;
                    default:
                        String msg = "Category not supported";
                        log.error(msg + ": {}", categoryName);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { categoryName });
                }
                engagementDto.setBookingId(Optional.of(bookingId));
                matcherCriteria = matcherCriteria.withMatcher("bookingId", match -> match.exact());
            } catch (BookingException e) {
                String action = "repository retrieval by category";
                String msg = "Unable to perform " + action;
                log.error(msg, e);
                throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { action + " failure: " + e.getMessage() });
            }
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(tokenNumber))) {
            log.debug("tokenNumber {} is valid", tokenNumber);
            providedFilters.put("tokenNumber", tokenNumber);
            engagementDto.setTokenNumber(Optional.of(tokenNumber));
            matcherCriteria = matcherCriteria.withMatcher("tokenNumber", match -> match.exact());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        List<? extends EngagementDocument> engagementDocumentList = engagementRepository.findAll(engagementDto, matcherCriteria);
        matchedEngagementList = reservationServiceHelper.engagementDocument2DetailedVo(engagementDocumentList);
        log.info("Found {} EngagementVo matching with provided parameters : {}", matchedEngagementList.size(), providedFilters);
        log.info("No EngagementVo available matching with provided parameters : {}", matchedEngagementList.size(), providedFilters);
        return matchedEngagementList;
    }

    @Override
    public String createDineInEngagement(EngagementForm form) throws EngagementException {
        log.info("Creating new EngagementDocument");

        if(form == null) {
            log.debug("EngagementForm provided is null");
            throw new EngagementException(ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of EngagementForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("EngagementForm has {} errors", err.getErrorCount());
            ReservationErrorCode ec = ReservationErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("EngagementForm error detail: {}", ec);
            throw new EngagementException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of EngagementForm are valid");

        Optional<? extends EngagementDocument> optionalEngagementDocument = form2DocumentConverter.convert(form);

        DineInEngagementDocument expectedDocument = (DineInEngagementDocument) optionalEngagementDocument.get();

        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_EXISTENCE_BY_BOOKING_ID_AND_TOKEN.getValue(),
                form.getBookingId(), form.getTokenNumber());
        if(dineInEngagementRepository.existsByBookingIdAndTokenNumberAndTableIdAndNoOfPersons(
                expectedDocument.getBookingId(), expectedDocument.getTokenNumber(), expectedDocument.gett(), expectedDocument.getTime())) {
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_EXISTS_BY_BOOKING_ID_AND_TOKEN.getValue(),
                    expectedDocument.getBookingId(), expectedDocument.getEvent(), expectedDocument.getDate(), expectedDocument.getTime());
            throw new EngagementException(ReservationErrorCode.RESERVATION_EXISTS,
                    new Object[]{ "bookingId: " + form.getBookingId() + ", event: " + form.getEvent(), ", date: " + form.getDate() + ", time: " + form.getTime() });
        }
        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_NON_EXISTENCE_BY_BOOKING_ID_AND_TOKEN.getValue(),
                expectedDocument.getBookingId(), expectedDocument.getEvent(), expectedDocument.getDate(), expectedDocument.getTime());

        log.debug("Saving {}", expectedDocument);
        EngagementDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null) {
            log.debug("Unable to create {}", expectedDocument);
            throw new EngagementException(ReservationErrorCode.RESERVATION_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist EngagementForm details" });
        }
        log.info("Created new EngagementForm with id: {}", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Override
    public String createTakeAwayEngagement(EngagementForm form) throws EngagementException {
        return null;
    }

    @Override
    public String createDeliveryEngagement(EngagementForm form) throws EngagementException {
        return null;
    }

    @Override
    public String createEngagement(EngagementForm form) throws EngagementException {
        log.info("Creating new EngagementDocument");

        if(form == null) {
            log.debug("EngagementForm provided is null");
            throw new EngagementException(ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of EngagementForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("EngagementForm has {} errors", err.getErrorCount());
            ReservationErrorCode ec = ReservationErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("EngagementForm error detail: {}", ec);
            throw new EngagementException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of EngagementForm are valid");

        Optional<? extends EngagementDocument> expectedDocument = form2DocumentConverter.convert(form);

        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_EXISTENCE_BY_BOOKING_ID_AND_TOKEN.getValue(),
                form.getBookingId(), form.getTokenNumber());
        if(dineInEngagementRepository.existsByBookingIdAndTokenNumberAndTableIdAndNoOfPersons(
                expectedDocument.getBookingId(), expectedDocument.getTokenNumber(), expectedDocument.gett(), expectedDocument.getTime())) {
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_EXISTS_BY_BOOKING_ID_AND_TOKEN.getValue(),
                    expectedDocument.getBookingId(), expectedDocument.getEvent(), expectedDocument.getDate(), expectedDocument.getTime());
            throw new EngagementException(ReservationErrorCode.RESERVATION_EXISTS,
                    new Object[]{ "bookingId: " + form.getBookingId() + ", event: " + form.getEvent(), ", date: " + form.getDate() + ", time: " + form.getTime() });
        }
        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_NON_EXISTENCE_BY_BOOKING_ID_AND_TOKEN.getValue(),
                expectedDocument.getBookingId(), expectedDocument.getEvent(), expectedDocument.getDate(), expectedDocument.getTime());

        log.debug("Saving {}", expectedDocument);
        EngagementDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null) {
            log.debug("Unable to create {}", expectedDocument);
            throw new EngagementException(ReservationErrorCode.RESERVATION_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist EngagementForm details" });
        }
        log.info("Created new EngagementForm with id: {}", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Override
    public void updateEngagement(String id, EngagementForm form) throws EngagementException {
        log.info("Updating EngagementForm by id: {}", id);

        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ENGAGEMENT_ENTITY_ID.getValue(), id);
        Optional<EngagementDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_NO_ENGAGEMENT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new EngagementException(ReservationErrorCode.RESERVATION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_FOUND_ENGAGEMENT_ENTITY_ID.getValue(), id);

        EngagementDocument actualDocument = optActualDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("EngagementDocument is inactive with id: {}", id);
            throw new EngagementException(ReservationErrorCode.RESERVATION_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("EngagementDocument is active with id: {}", id);

        if(form == null) {
            log.debug("EngagementForm is null");
            throw new EngagementException(ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of EngagementForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("EngagementForm has {} errors", err.getErrorCount());
            ReservationErrorCode ec = ReservationErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("EngagementForm error detail: {}", ec);
            throw new EngagementException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of EngagementForm are empty");
            throw new EngagementException(ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of EngagementForm are valid");

        Optional<EngagementDocument> optExpectedDocument = form2DocumentMapper.compareAndMap(actualDocument, form);
        if(optExpectedDocument.isEmpty()) {
            log.debug("No new value for attributes of EngagementForm");
            throw new EngagementException(ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from EngagementForm to EngagementDocument");

        EngagementDocument expectedDocument = optExpectedDocument.get();

        this.checkUniquenessOfEngagement(form, actualDocument);

        documentSelfMapper.compareAndMap(expectedDocument, actualDocument);
        log.debug("Compared and copied attributes from EngagementDocument to EngagementForm");
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Updated: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to update {}", actualDocument);
            throw new EngagementException(ReservationErrorCode.RESERVATION_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency engagement details" });
        }
        log.info("Updated existing EngagementDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void deleteEngagement(String id) throws EngagementException {
        log.info("Soft deleting EngagementDocument by id: {}", id);

        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ENGAGEMENT_ENTITY_ID.getValue(), id);
        Optional<EngagementDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_NO_ENGAGEMENT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new EngagementException(ReservationErrorCode.RESERVATION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_FOUND_ENGAGEMENT_ENTITY_ID.getValue(), id);

        EngagementDocument actualDocument = optDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("EngagementDocument is inactive with id: {}", id);
            throw new EngagementException(ReservationErrorCode.RESERVATION_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("EngagementDocument is active with id: {}", id);

        actualDocument.setActive(Boolean.FALSE);
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualDocument);
        EngagementDocument expectedDocument = repository.save(actualDocument);
        log.debug("Soft deleted: {}", expectedDocument);
        if(expectedDocument == null) {
            log.debug("Unable to soft delete {}", actualDocument);
            throw new EngagementException(ReservationErrorCode.RESERVATION_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current engagement details with id:" + id });
        }

        log.info("Soft deleted existing EngagementDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void applyPatchOnEngagement(String id, List<PatchOperationForm> patches) throws EngagementException {
        log.info("Patching EngagementDocument by id: {}", id);

        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ENGAGEMENT_ENTITY_ID.getValue(), id);
        Optional<EngagementDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_NO_ENGAGEMENT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new EngagementException(ReservationErrorCode.RESERVATION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_FOUND_ENGAGEMENT_ENTITY_ID.getValue(), id);

        EngagementDocument actualDocument = optActualDocument.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Engagement patch list not provided");
            throw new EngagementException(ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Engagement patch list has {} items", patches.size());


        log.debug("Validating patch list items for Engagement");
        try {
            toabBaseService.validatePatches(patches, ReservationErrorCode.RESERVATION_EXISTS.getDomain() + ":LOV");
            log.debug("All Engagement patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Engagement patch item are invalid");
            throw new EngagementException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Engagement");


        log.debug("Patching list items to EngagementDto");
        EngagementDto patchedEngagementForm = new EngagementDto();
        try {
            log.debug("Preparing patch list items for Engagement");
            JsonNode engagementDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch engagementPatch = JsonPatch.fromJson(engagementDtoTree);
            log.debug("Prepared patch list items for Engagement");
            JsonNode blankEngagementDtoTree = om.convertValue(new EngagementDto(), JsonNode.class);
            JsonNode patchedEngagementFormTree = engagementPatch.apply(blankEngagementDtoTree);
            log.debug("Applying patch list items to EngagementDto");
            patchedEngagementForm = om.treeToValue(patchedEngagementFormTree, EngagementDto.class);
            log.debug("Applied patch list items to EngagementDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to EngagementDto: {}", e);
            EngagementException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in EngagementDto");
                ex = new EngagementException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new EngagementException(ReservationErrorCode.RESERVATION_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to EngagementDto: {}", e);
            throw new EngagementException(ReservationErrorCode.RESERVATION_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to EngagementDto");

        log.debug("Validating patched EngagementDto");
        Errors err = new DirectFieldBindingResult(patchedEngagementForm, patchedEngagementForm.getClass().getSimpleName());
        dtoValidator.validate(patchedEngagementForm, err);
        if(err.hasErrors()) {
            log.debug("Patched EngagementDto has {} errors", err.getErrorCount());
            ReservationErrorCode ec = ReservationErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched EngagementDto error detail: {}", ec);
            throw new EngagementException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched EngagementDto are valid");

        this.checkUniquenessOfEngagement(patchedEngagementForm, actualDocument);

        log.debug("Comparatively copying patched attributes from EngagementDto to EngagementDocument");
        try {
            dto2DocumentConverter.compareAndMap(patchedEngagementForm, actualDocument);
        } catch (TOABBaseException e) {
            throw (EngagementException) e;
        }
        log.debug("Comparatively copied patched attributes from EngagementDto to EngagementDocument");

        log.debug("Saving patched EngagementDocument: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Saved patched EngagementDocument: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to patch delete EngagementDocument with id:{}", id);
            throw new EngagementException(ReservationErrorCode.RESERVATION_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency engagement details with id:" + id });
        }
        log.info("Patched EngagementDocument with id:{}", id);
    }

    private void checkUniquenessOfEngagement(EngagementDto patchedEngagementForm, EngagementDocument actualDocument) throws EngagementException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(2);
        if(patchedEngagementForm.getBookingId().isPresent()) {
            similaritySwitchesCollection.add(patchedEngagementForm.getBookingId().get().compareTo(actualDocument.getBookingId()) == 0);
        }
        if(patchedEngagementForm.getEvent().isPresent()) {
            similaritySwitchesCollection.add(patchedEngagementForm.getEvent().get().toUpperCase().compareTo(actualDocument.getEvent().name()) == 0);
        }
        if(patchedEngagementForm.getDate().isPresent()) {
            LocalDate date = LocalDate.parse(patchedEngagementForm.getDate().get(), DateTimeFormatter.ofPattern(dateFormat));
            similaritySwitchesCollection.add(date.compareTo(actualDocument.getDate()) == 0);
        }
        if(patchedEngagementForm.getTime().isPresent()) {
            LocalTime time = LocalTime.parse(patchedEngagementForm.getTime().get(), DateTimeFormatter.ofPattern(timeFormat));
            similaritySwitchesCollection.add(time.compareTo(actualDocument.getTime()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String bookingId = patchedEngagementForm.getBookingId().isPresent() ? patchedEngagementForm.getBookingId().get()
                    : actualDocument.getBookingId();
            EngagementEvent event = patchedEngagementForm.getEvent().isPresent() ? EngagementEvent.valueOf(patchedEngagementForm.getEvent().get())
                    : actualDocument.getEvent();
            LocalDate date = patchedEngagementForm.getDate().isPresent() ? LocalDate.parse(patchedEngagementForm.getDate().get(), DateTimeFormatter.ofPattern(dateFormat))
                    : actualDocument.getDate();
            LocalTime time = patchedEngagementForm.getTime().isPresent() ? LocalTime.parse(patchedEngagementForm.getTime().get(), DateTimeFormatter.ofPattern(timeFormat))
                    : actualDocument.getTime();
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_EXISTENCE_BY_BOOKING_ID_EVENT_DATE_TIME.getValue(), bookingId, event, date, time);
            boolean sameDocumentSw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateDocumentSw =  repository.existsByBookingIdAndEventAndDateAndTime(bookingId, event, date, time);
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_EXISTS_BY_BOOKING_ID_EVENT_DATE_TIME.getValue(), bookingId, event, date, time);
                throw new EngagementException(ReservationErrorCode.RESERVATION_EXISTS,
                        new Object[]{ "bookingId: " + bookingId + ", event: " + event, ", date: " + date + ", time: " + time });
            }
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_NON_EXISTENCE_BY_BOOKING_ID_EVENT_DATE_TIME.getValue(), bookingId, event, date, time);

        }
    }

    private void checkUniquenessOfEngagement(EngagementForm engagementForm, EngagementDocument actualDocument) throws EngagementException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(3);
        if(StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getBookingId()))) {
            similaritySwitchesCollection.add(engagementForm.getBookingId().compareTo(actualDocument.getBookingId()) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getEvent()))) {
            similaritySwitchesCollection.add(engagementForm.getEvent().toUpperCase().compareTo(actualDocument.getEvent().name()) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getDate()))) {
            LocalDate date = LocalDate.parse(engagementForm.getDate(), DateTimeFormatter.ofPattern(dateFormat));
            similaritySwitchesCollection.add(date.compareTo(actualDocument.getDate()) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getTime()))) {
            LocalTime time = LocalTime.parse(engagementForm.getTime(), DateTimeFormatter.ofPattern(timeFormat));
            similaritySwitchesCollection.add(time.compareTo(actualDocument.getTime()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String bookingId = StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getBookingId())) ? engagementForm.getBookingId() : actualDocument.getBookingId();
            EngagementEvent event = StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getEvent())) ? EngagementEvent.valueOf(engagementForm.getEvent())
                    : actualDocument.getEvent();
            LocalDate date = StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getDate())) ? LocalDate.parse(engagementForm.getDate(), DateTimeFormatter.ofPattern(dateFormat))
                    : actualDocument.getDate();
            LocalTime time = StringUtils.hasText(StringUtils.trimWhitespace(engagementForm.getTime())) ? LocalTime.parse(engagementForm.getTime(), DateTimeFormatter.ofPattern(timeFormat))
                    : actualDocument.getTime();

            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_EXISTENCE_BY_BOOKING_ID_EVENT_DATE_TIME.getValue(), bookingId, event, date, time);
            boolean sameDocumentSw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateDocumentSw =  repository.existsByBookingIdAndEventAndDateAndTime(bookingId, event, date, time);
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_EXISTS_BY_BOOKING_ID_EVENT_DATE_TIME.getValue(), bookingId, event, date, time);
                throw new EngagementException(ReservationErrorCode.RESERVATION_EXISTS,
                        new Object[]{ "bookingId: " + bookingId + ", event: " + event, ", date: " + date + ", time: " + time });
            }
            log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_NON_EXISTENCE_BY_BOOKING_ID_EVENT_DATE_TIME.getValue(), bookingId, event, date, time);

        }
    }
    
}