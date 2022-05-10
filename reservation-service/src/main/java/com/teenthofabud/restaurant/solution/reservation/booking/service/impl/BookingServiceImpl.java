package com.teenthofabud.restaurant.solution.reservation.booking.service.impl;

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
import com.teenthofabud.restaurant.solution.reservation.booking.converter.BookingDto2DocumentConverter;
import com.teenthofabud.restaurant.solution.reservation.booking.converter.BookingForm2DocumentConverter;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingDocument;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingDto;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingException;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingForm;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingMessageTemplate;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.reservation.booking.mapper.BookingDocumentSelfMapper;
import com.teenthofabud.restaurant.solution.reservation.booking.mapper.BookingForm2DocumentMapper;
import com.teenthofabud.restaurant.solution.reservation.booking.repository.BookingRepository;
import com.teenthofabud.restaurant.solution.reservation.booking.service.BookingService;
import com.teenthofabud.restaurant.solution.reservation.booking.validator.BookingDtoValidator;
import com.teenthofabud.restaurant.solution.reservation.booking.validator.BookingFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.reservation.booking.validator.BookingFormValidator;
import com.teenthofabud.restaurant.solution.reservation.error.ReservationErrorCode;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.reservation.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.reservation.integration.customer.validator.AccountIdValidator;
import com.teenthofabud.restaurant.solution.reservation.utils.ReservationServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.*;

@Component
@Slf4j
public class BookingServiceImpl implements BookingService {

    private static final Comparator<BookingVo> CMP_BY_CATEGORY_TABLE_ACCOUNT = (s1, s2) -> {
       int c1 = s1.getCategoryId().compareTo(s2.getCategoryId());
       if(c1 == 0) {
           int c2 = s1.getTimestamp().compareTo(s2.getTimestamp());
           if(c2 == 0) {
               int c3 = s1.getAccountId().compareTo(s2.getAccountId());
               return c3;
           } else {
               return c2;
           }
       } else {
           return c1;
       }
    };

    private BookingForm2DocumentConverter form2DocumentConverter;
    private BookingDto2DocumentConverter dto2DocumentConverter;
    private BookingForm2DocumentMapper form2DocumentMapper;
    private BookingDocumentSelfMapper documentSelfMapper;
    private BookingFormValidator formValidator;
    private BookingFormRelaxedValidator relaxedFormValidator;
    private BookingDtoValidator dtoValidator;
    private BookingRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private ReservationServiceHelper reservationServiceHelper;
    private CategoryService categoryService;
    private AccountIdValidator accountIdValidator;
    private String bookingTimeFormat;

    @Value("${res.reservation.booking.timestamp}")
    public void setBookingTimeFormat(String bookingTimeFormat) {
        this.bookingTimeFormat = bookingTimeFormat;
    }

    @Autowired
    public void setAccountIdValidator(AccountIdValidator accountIdValidator) {
        this.accountIdValidator = accountIdValidator;
    }

    @Autowired
    public void setReservationServiceHelper(ReservationServiceHelper reservationServiceHelper) {
        this.reservationServiceHelper = reservationServiceHelper;
    }

    @Autowired
    public void setCategoryService(CategoryService categoryService) {
        this.categoryService = categoryService;
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
    public void setDto2DocumentConverter(BookingDto2DocumentConverter dto2DocumentConverter) {
        this.dto2DocumentConverter = dto2DocumentConverter;
    }

    @Autowired
    public void setForm2DocumentMapper(BookingForm2DocumentMapper form2DocumentMapper) {
        this.form2DocumentMapper = form2DocumentMapper;
    }

    @Autowired
    public void setDocumentSelfMapper(BookingDocumentSelfMapper documentSelfMapper) {
        this.documentSelfMapper = documentSelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(BookingFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchBookingValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(BookingDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2DocumentConverter(BookingForm2DocumentConverter form2DocumentConverter) {
        this.form2DocumentConverter = form2DocumentConverter;
    }

    @Autowired
    public void setRepository(BookingRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(BookingFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseBookingId(String id) throws BookingException {
        Long bookingId = null;
        try {
            bookingId = Long.parseLong(id);
            log.debug("Parsed id {} to booking id {} in numeric format", id, bookingId);
            if(bookingId <= 0) {
                throw new NumberFormatException("booking id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse booking id", e);
            log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_ID_INVALID.getValue(), id);
            throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return bookingId;
    }

    @Override
    public Set<BookingVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all BookingDocument by their natural ordering");
        List<BookingDocument> bookingDocumentList = repository.findAll();
        List<BookingVo> bookingVoList = reservationServiceHelper.bookingDocument2DetailedVo(bookingDocumentList);
        Set<BookingVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_CATEGORY_TABLE_ACCOUNT);
        naturallyOrderedSet.addAll(bookingVoList);
        log.info("{} BookingVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public BookingVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws BookingException {
        log.info("Requesting BookingDocument by id: {}", id);
        Optional<BookingDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug("No BookingDocument found by id: {}", id);
            throw new BookingException(ReservationErrorCode.RESERVATION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found BookingVo by id: {}", id);
        BookingDocument document = optDocument.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        BookingVo vo = reservationServiceHelper.bookingDocument2DetailedVo(document);
        log.debug("BookingVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<BookingVo> retrieveAllMatchingDetailsByCategoryId(String categoryId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws BookingException {
        log.info("Requesting BookingDocument that match with categoryId: {}", categoryId);
        Errors err = new DirectFieldBindingResult(categoryId, "BookingForm");
        try {
            CategoryVo categoryVo = categoryService.retrieveDetailsById(categoryId, Optional.of(TOABCascadeLevel.ONE));
            if(!categoryVo.getActive()) {
                throw new CategoryException(ReservationErrorCode.RESERVATION_INACTIVE, new Object [] { categoryId });
            }
        } catch (CategoryException e) {
            log.error("categoryId is invalid", e);
            throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object [] { "categoryId: " + categoryId });
        }
        List<BookingDocument> bookingDocumentList = repository.findByCategoryId(categoryId);
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        List<BookingVo> matchedBookingList = reservationServiceHelper.bookingDocument2DetailedVo(bookingDocumentList);
        log.info("Found {} BookingVo matching with recipeId: {}", matchedBookingList.size(), categoryId);
        TOABRequestContextHolder.clearCascadeLevelContext();
        if(matchedBookingList.isEmpty()) {
            log.debug("No BookingVo found matching with categoryId: {}", categoryId);
            throw new BookingException(ReservationErrorCode.RESERVATION_NOT_FOUND, new Object[] { "categoryId", categoryId });
        }
        return matchedBookingList;
    }

    @Deprecated
    @Override
    public List<BookingVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalTimestamp, Optional<String> optionalAccountId) throws BookingException {
        if(optionalTimestamp.isEmpty() && optionalAccountId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String timestamp = optionalTimestamp.isPresent() ? optionalTimestamp.get() : "";
        String accountId = optionalAccountId.isPresent() ? optionalAccountId.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(timestamp)) && StringUtils.isEmpty(StringUtils.trimWhitespace(accountId))) {
            log.debug("All search parameters are empty");
        }
        List<BookingVo> matchedBookingList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        BookingDocument document = new BookingDocument();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(timestamp))) {
            log.debug("timestamp {} is valid", timestamp);
            providedFilters.put("timestamp", timestamp);
            document.setTimestamp(LocalDateTime.parse(timestamp, DateTimeFormatter.ofPattern(bookingTimeFormat)));
            matcherCriteria = matcherCriteria.withMatcher("timestamp", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(accountId))) {
            log.debug("accountId {} is valid", accountId);
            providedFilters.put("accountId", accountId);
            document.setAccountId(accountId);
            matcherCriteria = matcherCriteria.withMatcher("accountId", match -> match.exact());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<BookingDocument> bookingDocumentExample = Example.of(document, matcherCriteria);
        List<BookingDocument> bookingDocumentList = repository.findAll(bookingDocumentExample);
        matchedBookingList = reservationServiceHelper.bookingDocument2DetailedVo(bookingDocumentList);
        log.info("Found {} BookingVo matching with provided parameters : {}", matchedBookingList.size(), providedFilters);
        log.info("No BookingVo available matching with provided parameters : {}", matchedBookingList.size(), providedFilters);
        return matchedBookingList;
    }

    @Override
    public List<BookingVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalTimestamp, Optional<String> optionalAccountId, Optional<String> optionalCategoryId) throws BookingException {
        if(optionalTimestamp.isEmpty() && optionalAccountId.isEmpty() && optionalCategoryId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String timestamp = optionalTimestamp.isPresent() ? optionalTimestamp.get() : "";
        String accountId = optionalAccountId.isPresent() ? optionalAccountId.get() : "";
        String categoryId = optionalCategoryId.isPresent() ? optionalCategoryId.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(timestamp)) && StringUtils.isEmpty(StringUtils.trimWhitespace(accountId))
            && StringUtils.isEmpty(StringUtils.trimWhitespace(categoryId))) {
            log.debug("All search parameters are empty");
        }
        List<BookingVo> matchedBookingList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        BookingDocument document = new BookingDocument();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(timestamp))) {
            LocalDateTime ts = null;
            try {
                ts = LocalDateTime.parse(timestamp, DateTimeFormatter.ofPattern(bookingTimeFormat));
            } catch (DateTimeParseException e) {
                log.error("timestamp is invalid: {}", e);
                throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[] { "timestamp", timestamp });
            }
            log.debug("timestamp {} is valid", timestamp);
            providedFilters.put("timestamp", timestamp);
            document.setTimestamp(ts);
            matcherCriteria = matcherCriteria.withMatcher("timestamp", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(accountId))) {
            log.debug("accountId {} is valid", accountId);
            providedFilters.put("accountId", accountId);
            document.setAccountId(accountId);
            matcherCriteria = matcherCriteria.withMatcher("accountId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(categoryId))) {
            log.debug("categoryId {} is valid", categoryId);
            providedFilters.put("categoryId", categoryId);
            document.setCategoryId(categoryId);
            matcherCriteria = matcherCriteria.withMatcher("categoryId", match -> match.exact());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<BookingDocument> bookingDocumentExample = Example.of(document, matcherCriteria);
        List<BookingDocument> bookingDocumentList = repository.findAll(bookingDocumentExample);
        matchedBookingList = reservationServiceHelper.bookingDocument2DetailedVo(bookingDocumentList);
        log.info("Found {} BookingVo matching with provided parameters : {}", matchedBookingList.size(), providedFilters);
        log.info("No BookingVo available matching with provided parameters : {}", matchedBookingList.size(), providedFilters);
        return matchedBookingList;
    }

    @Override
    public String createBooking(BookingForm form) throws BookingException {
        log.info("Creating new BookingDocument");

        if(form == null) {
            log.debug("BookingForm provided is null");
            throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of BookingForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("BookingForm has {} errors", err.getErrorCount());
            ReservationErrorCode ec = ReservationErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("BookingForm error detail: {}", ec);
            throw new BookingException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of BookingForm are valid");

        BookingDocument expectedDocument = form2DocumentConverter.convert(form);

        log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_EXISTENCE_BY_CATEGORY_ID_AND_TIMESTAMP_AND_ACCOUNT_ID.getValue(), form.getAccountId(), form.getTimestamp(), form.getCategoryId());
        if(repository.existsByAccountIdAndTimestampAndCategoryId(expectedDocument.getAccountId(), expectedDocument.getTimestamp(), expectedDocument.getCategoryId())) {
            log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_EXISTS_BY_CATEGORY_ID_AND_TIMESTAMP_AND_ACCOUNT_ID.getValue(), expectedDocument.getAccountId(), expectedDocument.getTimestamp(), expectedDocument.getCategoryId());
            throw new BookingException(ReservationErrorCode.RESERVATION_EXISTS,
                    new Object[]{ "accountId: " + form.getAccountId(), "timestamp: " + form.getTimestamp() + ", categoryId: " + form.getCategoryId() });
        }
        log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_NON_EXISTENCE_BY_CATEGORY_ID_AND_TIMESTAMP_AND_ACCOUNT_ID.getValue(), expectedDocument.getAccountId(), expectedDocument.getTimestamp(), expectedDocument.getCategoryId());

        log.debug("Saving {}", expectedDocument);
        BookingDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null) {
            log.debug("Unable to create {}", expectedDocument);
            throw new BookingException(ReservationErrorCode.RESERVATION_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist BookingForm details" });
        }
        log.info("Created new BookingForm with id: {}", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Override
    public void updateBooking(String id, BookingForm form) throws BookingException {
        log.info("Updating BookingForm by id: {}", id);

        log.debug(BookingMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_BOOKING_ENTITY_ID.getValue(), id);
        Optional<BookingDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(BookingMessageTemplate.MSG_TEMPLATE_NO_BOOKING_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new BookingException(ReservationErrorCode.RESERVATION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(BookingMessageTemplate.MSG_TEMPLATE_FOUND_BOOKING_ENTITY_ID.getValue(), id);

        BookingDocument actualDocument = optActualDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("BookingDocument is inactive with id: {}", id);
            throw new BookingException(ReservationErrorCode.RESERVATION_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("BookingDocument is active with id: {}", id);

        if(form == null) {
            log.debug("BookingForm is null");
            throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of BookingForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("BookingForm has {} errors", err.getErrorCount());
            ReservationErrorCode ec = ReservationErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("BookingForm error detail: {}", ec);
            throw new BookingException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of BookingForm are empty");
            throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of BookingForm are valid");

        Optional<BookingDocument> optExpectedDocument = form2DocumentMapper.compareAndMap(actualDocument, form);
        if(optExpectedDocument.isEmpty()) {
            log.debug("No new value for attributes of BookingForm");
            throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from BookingForm to BookingDocument");

        BookingDocument expectedDocument = optExpectedDocument.get();

        this.checkUniquenessOfBooking(form, actualDocument);

        documentSelfMapper.compareAndMap(expectedDocument, actualDocument);
        log.debug("Compared and copied attributes from BookingDocument to BookingForm");
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Updated: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to update {}", actualDocument);
            throw new BookingException(ReservationErrorCode.RESERVATION_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency booking details" });
        }
        log.info("Updated existing BookingDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void deleteBooking(String id) throws BookingException {
        log.info("Soft deleting BookingDocument by id: {}", id);

        log.debug(BookingMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_BOOKING_ENTITY_ID.getValue(), id);
        Optional<BookingDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug(BookingMessageTemplate.MSG_TEMPLATE_NO_BOOKING_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new BookingException(ReservationErrorCode.RESERVATION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(BookingMessageTemplate.MSG_TEMPLATE_FOUND_BOOKING_ENTITY_ID.getValue(), id);

        BookingDocument actualDocument = optDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("BookingDocument is inactive with id: {}", id);
            throw new BookingException(ReservationErrorCode.RESERVATION_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("BookingDocument is active with id: {}", id);

        actualDocument.setActive(Boolean.FALSE);
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualDocument);
        BookingDocument expectedDocument = repository.save(actualDocument);
        log.debug("Soft deleted: {}", expectedDocument);
        if(expectedDocument == null) {
            log.debug("Unable to soft delete {}", actualDocument);
            throw new BookingException(ReservationErrorCode.RESERVATION_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current booking details with id:" + id });
        }

        log.info("Soft deleted existing BookingDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void applyPatchOnBooking(String id, List<PatchOperationForm> patches) throws BookingException {
        log.info("Patching BookingDocument by id: {}", id);

        log.debug(BookingMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_BOOKING_ENTITY_ID.getValue(), id);
        Optional<BookingDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(BookingMessageTemplate.MSG_TEMPLATE_NO_BOOKING_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new BookingException(ReservationErrorCode.RESERVATION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(BookingMessageTemplate.MSG_TEMPLATE_FOUND_BOOKING_ENTITY_ID.getValue(), id);

        BookingDocument actualDocument = optActualDocument.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Booking patch list not provided");
            throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Booking patch list has {} items", patches.size());


        log.debug("Validating patch list items for Booking");
        try {
            toabBaseService.validatePatches(patches, ReservationErrorCode.RESERVATION_EXISTS.getDomain() + ":LOV");
            log.debug("All Booking patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Booking patch item are invalid");
            throw new BookingException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Booking");


        log.debug("Patching list items to BookingDto");
        BookingDto patchedBookingForm = new BookingDto();
        try {
            log.debug("Preparing patch list items for Booking");
            JsonNode bookingDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch bookingPatch = JsonPatch.fromJson(bookingDtoTree);
            log.debug("Prepared patch list items for Booking");
            JsonNode blankBookingDtoTree = om.convertValue(new BookingDto(), JsonNode.class);
            JsonNode patchedBookingFormTree = bookingPatch.apply(blankBookingDtoTree);
            log.debug("Applying patch list items to BookingDto");
            patchedBookingForm = om.treeToValue(patchedBookingFormTree, BookingDto.class);
            log.debug("Applied patch list items to BookingDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to BookingDto: {}", e);
            BookingException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in BookingDto");
                ex = new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new BookingException(ReservationErrorCode.RESERVATION_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to BookingDto: {}", e);
            throw new BookingException(ReservationErrorCode.RESERVATION_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to BookingDto");

        log.debug("Validating patched BookingDto");
        Errors err = new DirectFieldBindingResult(patchedBookingForm, patchedBookingForm.getClass().getSimpleName());
        dtoValidator.validate(patchedBookingForm, err);
        if(err.hasErrors()) {
            log.debug("Patched BookingDto has {} errors", err.getErrorCount());
            ReservationErrorCode ec = ReservationErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched BookingDto error detail: {}", ec);
            throw new BookingException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched BookingDto are valid");

        this.checkUniquenessOfBooking(patchedBookingForm, actualDocument);

        log.debug("Comparatively copying patched attributes from BookingDto to BookingDocument");
        try {
            dto2DocumentConverter.compareAndMap(patchedBookingForm, actualDocument);
        } catch (TOABBaseException e) {
            throw (BookingException) e;
        }
        log.debug("Comparatively copied patched attributes from BookingDto to BookingDocument");

        log.debug("Saving patched BookingDocument: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Saved patched BookingDocument: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to patch delete BookingDocument with id:{}", id);
            throw new BookingException(ReservationErrorCode.RESERVATION_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency booking details with id:" + id });
        }
        log.info("Patched BookingDocument with id:{}", id);
    }

    private void checkUniquenessOfBooking(BookingDto patchedBookingForm, BookingDocument actualDocument) throws BookingException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(2);
        if(patchedBookingForm.getAccountId().isPresent()) {
            similaritySwitchesCollection.add(patchedBookingForm.getAccountId().get().compareTo(actualDocument.getAccountId()) == 0);
        }
        if(patchedBookingForm.getTimestamp().isPresent()) {
            similaritySwitchesCollection.add(patchedBookingForm.getTimestamp().get().compareTo(
                    DateTimeFormatter.ofPattern(bookingTimeFormat).format(actualDocument.getTimestamp())) == 0);
        }
        if(patchedBookingForm.getCategoryId().isPresent()) {
            similaritySwitchesCollection.add(patchedBookingForm.getCategoryId().get().compareTo(actualDocument.getCategoryId()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String accountId = patchedBookingForm.getAccountId().isPresent() ? patchedBookingForm.getAccountId().get() : actualDocument.getAccountId();
           /* String timestamp = patchedBookingForm.getTimestamp().isPresent() ? patchedBookingForm.getTimestamp().get()
                    : DateTimeFormatter.ofPattern(bookingTimeFormat).format(actualDocument.getTimestamp());*/
            LocalDateTime timestamp = patchedBookingForm.getTimestamp().isPresent()
                    ? LocalDateTime.parse(patchedBookingForm.getTimestamp().get(), DateTimeFormatter.ofPattern(bookingTimeFormat))
                    : actualDocument.getTimestamp();
            String categoryId = patchedBookingForm.getCategoryId().isPresent() ? patchedBookingForm.getCategoryId().get() : actualDocument.getCategoryId();
            log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_EXISTENCE_BY_CATEGORY_ID_AND_TIMESTAMP_AND_ACCOUNT_ID.getValue(), accountId, timestamp, categoryId);
            boolean sameDocumentSw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateDocumentSw =  repository.existsByAccountIdAndTimestampAndCategoryId(accountId, timestamp, categoryId);
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_EXISTS_BY_CATEGORY_ID_AND_TIMESTAMP_AND_ACCOUNT_ID.getValue(), timestamp, accountId);
                throw new BookingException(ReservationErrorCode.RESERVATION_EXISTS, new Object[]{ "accountId: " + accountId,
                        ", timestamp: " + timestamp.format(DateTimeFormatter.ofPattern(bookingTimeFormat)) + ", categoryId: " + categoryId });
            }
            log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_NON_EXISTENCE_BY_CATEGORY_ID_AND_TIMESTAMP_AND_ACCOUNT_ID.getValue(), accountId, timestamp, categoryId);

        }
    }

    private void checkUniquenessOfBooking(BookingForm bookingForm, BookingDocument actualDocument) throws BookingException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(3);
        if(StringUtils.hasText(StringUtils.trimWhitespace(bookingForm.getAccountId()))) {
            similaritySwitchesCollection.add(bookingForm.getAccountId().compareTo(actualDocument.getAccountId()) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(bookingForm.getTimestamp()))) {
            similaritySwitchesCollection.add(bookingForm.getTimestamp().compareTo(
                    DateTimeFormatter.ofPattern(bookingTimeFormat).format(actualDocument.getTimestamp())) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(bookingForm.getCategoryId()))) {
            similaritySwitchesCollection.add(bookingForm.getCategoryId().compareTo(actualDocument.getCategoryId()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String accountId = StringUtils.hasText(StringUtils.trimWhitespace(bookingForm.getAccountId())) ? bookingForm.getAccountId() : actualDocument.getAccountId();
            LocalDateTime timestamp = StringUtils.hasText(StringUtils.trimWhitespace(bookingForm.getTimestamp()))
                    ? LocalDateTime.parse(bookingForm.getTimestamp(), DateTimeFormatter.ofPattern(bookingTimeFormat))
                    : actualDocument.getTimestamp();
            String categoryId = StringUtils.hasText(StringUtils.trimWhitespace(bookingForm.getCategoryId())) ? bookingForm.getCategoryId() : actualDocument.getCategoryId();
            log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_EXISTENCE_BY_CATEGORY_ID_AND_TIMESTAMP_AND_ACCOUNT_ID.getValue(), accountId, timestamp, categoryId);
            boolean sameDocumentSw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateDocumentSw =  repository.existsByAccountIdAndTimestampAndCategoryId(accountId, timestamp, categoryId);
            if(sameDocumentSw || duplicateDocumentSw) {
                log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_EXISTS_BY_CATEGORY_ID_AND_TIMESTAMP_AND_ACCOUNT_ID.getValue(), accountId, timestamp, categoryId);
                throw new BookingException(ReservationErrorCode.RESERVATION_EXISTS, new Object[]{ "accountId: " + accountId,
                        ", timestamp: " + timestamp.format(DateTimeFormatter.ofPattern(bookingTimeFormat)) + ", categoryId: " + categoryId });
            }
            log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_NON_EXISTENCE_BY_CATEGORY_ID_AND_TIMESTAMP_AND_ACCOUNT_ID.getValue(), accountId, timestamp, categoryId);

        }
    }
    
}