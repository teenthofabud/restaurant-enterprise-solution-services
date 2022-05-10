package com.teenthofabud.restaurant.solution.reservation.booking.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingException;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingForm;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingMessageTemplate;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.reservation.booking.service.BookingService;
import com.teenthofabud.restaurant.solution.reservation.error.ReservationErrorCode;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@RestController
@RequestMapping("booking")
@Slf4j
@Tag(name = "Booking API", description = "Manage Bookings and their details")
public class BookingController {

    private static final String MEDIA_BOOKING_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(BookingService service) {
        this.service = service;
    }

    private BookingService service;

    @Operation(summary = "Create new Booking details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Booking",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Booking attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Booking already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Booking attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Booking",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewBooking(@RequestBody(required = false) BookingForm form) throws BookingException {
        log.debug("Requesting to create new booking");
        if(form != null) {
            String id = service.createBooking(form);
            log.debug("Responding with identifier of newly created new booking");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("BookingForm is null");
        throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Booking details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Booking",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Booking attribute's value is invalid/Booking is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Booking found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Booking already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Booking details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingBooking(@PathVariable String id, @RequestBody(required = false) BookingForm form) throws BookingException {
        log.debug("Requesting to update all attributes of existing booking");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateBooking(id, form);
                log.debug("Responding with successful updation of attributes for existing booking");
                return;
            }
            log.debug("BookingForm is null");
            throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_ID_EMPTY.getValue());
        throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Booking by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Booking",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Booking id is invalid/Booking is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Booking found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Booking attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Booking",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingBooking(@PathVariable String id) throws BookingException {
        log.debug("Requesting to soft delete booking");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteBooking(id);
            log.debug("Responding with successful deletion of existing booking");
            return;
        }
        log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_ID_EMPTY.getValue());
        throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Booking attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Booking with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Booking attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Booking found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Booking attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Booking with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_BOOKING_APPLICATION_JSON_PATCH)
    public void patchExistingBooking(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws BookingException {
        log.debug("Requesting to patch of booking attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnBooking(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing booking");
                return;
            }
            log.debug("booking patch document is null");
            throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_ID_EMPTY.getValue());
        throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Booking details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Bookings and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = BookingVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<BookingVo> getAllBookingNaturallyOrdered() {
        log.debug("Requesting all available bookings by their natural orders");
        Set<BookingVo> naturallyOrderedBookings = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available bookings by their natural orders");
        return naturallyOrderedBookings;
    }

    @Operation(summary = "Get all Booking details by accountId, timestamp")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Bookings and their details that match the provided accountId, timestamp",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = BookingVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Booking search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Bookings available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<BookingVo> getAllBookingsByFilters(@RequestParam(required = false) String accountId,
                                                   @RequestParam(required = false) String categoryId,
                                                       @RequestParam(required = false) String timestamp) throws BookingException {
        log.debug("Requesting all available bookings with given filters");
        boolean emptyAccountId = !StringUtils.hasText(StringUtils.trimWhitespace(accountId));
        boolean emptyCategoryId = !StringUtils.hasText(StringUtils.trimWhitespace(categoryId));
        boolean emptyTimestamp = !StringUtils.hasText(StringUtils.trimWhitespace(timestamp));
        if(!emptyAccountId || !emptyCategoryId || !emptyTimestamp) {
            Optional<String> optAccountId = emptyAccountId ? Optional.empty() : Optional.of(accountId);
            Optional<String> optCategoryId = emptyCategoryId ? Optional.empty() : Optional.of(categoryId);
            Optional<String> optTimestamp = emptyTimestamp ? Optional.empty() : Optional.of(timestamp);
            List<BookingVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optTimestamp, optAccountId, optCategoryId);
            log.debug("Responding with all available bookings with given filters");
            return matchedByFilter;
        }
        log.debug("booking filters are empty");
        throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get all Booking details by category id", hidden = true)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Bookinges and their details that match the given category id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = BookingVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Booking id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Bookinges available with the given category id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("categoryid/{categoryId}")
    public List<BookingVo> getAllBookingsByCategoryId(@PathVariable String categoryId, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws BookingException {
        List<BookingVo> matchedByCategoryIds = new ArrayList<>();
        log.debug("Requesting all available bookinges with given categoryId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(categoryId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            matchedByCategoryIds = service.retrieveAllMatchingDetailsByCategoryId(categoryId, Optional.empty());
            log.debug("Responding with all available bookinges with given categoryId");
            return matchedByCategoryIds;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(categoryId)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                matchedByCategoryIds = service.retrieveAllMatchingDetailsByCategoryId(categoryId, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing booking details with given categoryId having fields cascaded to given level");
                return matchedByCategoryIds;
            } catch (NumberFormatException e) {
                log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_CASCADE_LEVEL_EMPTY.getValue());
                throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug("booking categoryId is empty");
        throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[] { "categoryId", categoryId });
    }

    @Operation(summary = "Get Booking details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Booking that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = BookingVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Booking id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Booking found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public BookingVo getBookingDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws BookingException {
        BookingVo bookingDetails = null;
        log.debug("Requesting all details of booking by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            bookingDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing booking details by id");
            return bookingDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                bookingDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing booking details by id wth fields cascaded to given level");
                return bookingDetails;
            } catch (NumberFormatException e) {
                log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_CASCADE_LEVEL_EMPTY.getValue());
                throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(BookingMessageTemplate.MSG_TEMPLATE_BOOKING_ID_EMPTY.getValue());
        throw new BookingException(ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
