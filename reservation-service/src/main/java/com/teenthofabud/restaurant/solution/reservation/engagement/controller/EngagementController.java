package com.teenthofabud.restaurant.solution.reservation.engagement.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementException;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementForm;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementMessageTemplate;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementVo;
import com.teenthofabud.restaurant.solution.reservation.engagement.service.EngagementService;
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
@RequestMapping("engagement")
@Slf4j
@Tag(name = "Engagement API", description = "Manage Engagements and their details")
public class EngagementController {

    private static final String MEDIA_BOOKING_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(EngagementService service) {
        this.service = service;
    }

    private EngagementService service;

    @Operation(summary = "Create new Engagement details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Engagement",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Engagement attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Engagement already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Engagement attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Engagement",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewEngagement(@RequestBody(required = false) EngagementForm form) throws EngagementException {
        log.debug("Requesting to create new engagement");
        if(form != null) {
            String id = service.createEngagement(form);
            log.debug("Responding with identifier of newly created new engagement");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("EngagementForm is null");
        throw new EngagementException(ReservationErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Engagement details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Engagement",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Engagement attribute's value is invalid/Engagement is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Engagement found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Engagement already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Engagement details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingEngagement(@PathVariable String id, @RequestBody(required = false) EngagementForm form) throws EngagementException {
        log.debug("Requesting to update all attributes of existing engagement");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateEngagement(id, form);
                log.debug("Responding with successful updation of attributes for existing engagement");
                return;
            }
            log.debug("EngagementForm is null");
            throw new EngagementException(ReservationErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_ID_EMPTY.getValue());
        throw new EngagementException(ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Engagement by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Engagement",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Engagement id is invalid/Engagement is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Engagement found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Engagement attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Engagement",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingEngagement(@PathVariable String id) throws EngagementException {
        log.debug("Requesting to soft delete engagement");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteEngagement(id);
            log.debug("Responding with successful deletion of existing engagement");
            return;
        }
        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_ID_EMPTY.getValue());
        throw new EngagementException(ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Engagement attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Engagement with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Engagement attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Engagement found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Engagement attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Engagement with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_BOOKING_APPLICATION_JSON_PATCH)
    public void patchExistingEngagement(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws EngagementException {
        log.debug("Requesting to patch of engagement attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnEngagement(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing engagement");
                return;
            }
            log.debug("engagement patch document is null");
            throw new EngagementException(ReservationErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_ID_EMPTY.getValue());
        throw new EngagementException(ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Engagement details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Engagements and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = EngagementVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<EngagementVo> getAllEngagementNaturallyOrdered() {
        log.debug("Requesting all available engagements by their natural orders");
        Set<EngagementVo> naturallyOrderedEngagements = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available engagements by their natural orders");
        return naturallyOrderedEngagements;
    }

    @Operation(summary = "Get all Engagement details by associationId, event, date, time")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Engagements and their details that match the provided associationId, event, date, time",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = EngagementVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Engagement search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Engagements available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<EngagementVo> getAllEngagementsByFilters(@RequestParam(required = false) String associationId,
                                                         @RequestParam(required = false) String event,
                                                         @RequestParam(required = false) String timestamp) throws EngagementException {
        log.debug("Requesting all available engagements with given filters");
        boolean emptyAssociationId = !StringUtils.hasText(StringUtils.trimWhitespace(associationId));
        boolean emptyEvent = !StringUtils.hasText(StringUtils.trimWhitespace(event));
        boolean emptyTimestamp = !StringUtils.hasText(StringUtils.trimWhitespace(timestamp));
        if(!emptyEvent || !emptyAssociationId || !emptyTimestamp) {
            Optional<String> optAssociationId = emptyAssociationId ? Optional.empty() : Optional.of(associationId);
            Optional<String> optEvent = emptyEvent ? Optional.empty() : Optional.of(event);
            Optional<String> optTimestamp = emptyTimestamp ? Optional.empty() : Optional.of(timestamp);
            List<EngagementVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optAssociationId, optEvent, optTimestamp);
            log.debug("Responding with all available engagements with given filters");
            return matchedByFilter;
        }
        log.debug("engagement filters are empty");
        throw new EngagementException(ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get all Engagement details by booking id", hidden = true)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Engagementes and their details that match the given booking id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = EngagementVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Engagement id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Engagementes available with the given booking id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("associationid/{associationId}")
    public List<EngagementVo> getAllEngagementsByAssociationId(@PathVariable String associationId, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws EngagementException {
        List<EngagementVo> matchedByAssociationIds = new ArrayList<>();
        log.debug("Requesting all available engagementes with given associationId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(associationId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            matchedByAssociationIds = service.retrieveAllMatchingDetailsByAssociationId(associationId, Optional.empty());
            log.debug("Responding with all available engagementes with given associationId");
            return matchedByAssociationIds;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(associationId)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                matchedByAssociationIds = service.retrieveAllMatchingDetailsByAssociationId(associationId, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing engagement details with given associationId having fields cascaded to given level");
                return matchedByAssociationIds;
            } catch (NumberFormatException e) {
                log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_CASCADE_LEVEL_EMPTY.getValue());
                throw new EngagementException(ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug("engagement associationId is empty");
        throw new EngagementException(ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "associationId", associationId });
    }

    @Operation(summary = "Get Engagement details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Engagement that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = EngagementVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Engagement id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Engagement found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public EngagementVo getEngagementDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws EngagementException {
        EngagementVo engagementDetails = null;
        log.debug("Requesting all details of engagement by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            engagementDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing engagement details by id");
            return engagementDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                engagementDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing engagement details by id wth fields cascaded to given level");
                return engagementDetails;
            } catch (NumberFormatException e) {
                log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_CASCADE_LEVEL_EMPTY.getValue());
                throw new EngagementException(ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(EngagementMessageTemplate.MSG_TEMPLATE_ENGAGEMENT_ID_EMPTY.getValue());
        throw new EngagementException(ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}