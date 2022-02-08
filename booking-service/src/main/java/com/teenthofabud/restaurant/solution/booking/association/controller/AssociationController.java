package com.teenthofabud.restaurant.solution.booking.association.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationException;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationForm;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationMessageTemplate;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationVo;
import com.teenthofabud.restaurant.solution.booking.association.service.AssociationService;
import com.teenthofabud.restaurant.solution.booking.error.BookingErrorCode;
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
@RequestMapping("association")
@Slf4j
@Tag(name = "Association API", description = "Manage Associations and their details")
public class AssociationController {

    private static final String MEDIA_BOOKING_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(AssociationService service) {
        this.service = service;
    }

    private AssociationService service;

    @Operation(summary = "Create new Association details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Association",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Association attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Association already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Association attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Association",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewAssociation(@RequestBody(required = false) AssociationForm form) throws AssociationException {
        log.debug("Requesting to create new association");
        if(form != null) {
            String id = service.createAssociation(form);
            log.debug("Responding with identifier of newly created new association");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("AssociationForm is null");
        throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Association details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Association",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Association attribute's value is invalid/Association is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Association found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Association already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Association details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingAssociation(@PathVariable String id, @RequestBody(required = false) AssociationForm form) throws AssociationException {
        log.debug("Requesting to update all attributes of existing association");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateAssociation(id, form);
                log.debug("Responding with successful updation of attributes for existing association");
                return;
            }
            log.debug("AssociationForm is null");
            throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_ID_EMPTY.getValue());
        throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Association by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Association",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Association id is invalid/Association is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Association found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Association attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Association",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingAssociation(@PathVariable String id) throws AssociationException {
        log.debug("Requesting to soft delete association");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteAssociation(id);
            log.debug("Responding with successful deletion of existing association");
            return;
        }
        log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_ID_EMPTY.getValue());
        throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Association attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Association with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Association attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Association found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Association attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Association with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_BOOKING_APPLICATION_JSON_PATCH)
    public void patchExistingAssociation(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws AssociationException {
        log.debug("Requesting to patch of association attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnAssociation(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing association");
                return;
            }
            log.debug("association patch document is null");
            throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_ID_EMPTY.getValue());
        throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Association details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Associations and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = AssociationVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<AssociationVo> getAllAssociationNaturallyOrdered() {
        log.debug("Requesting all available associations by their natural orders");
        Set<AssociationVo> naturallyOrderedAssociations = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available associations by their natural orders");
        return naturallyOrderedAssociations;
    }

    @Operation(summary = "Get all Association details by tableId, experienceId")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Associations and their details that match the provided tableId, experienceId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = AssociationVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Association search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Associations available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<AssociationVo> getAllAssociationsByFilters(@RequestParam(required = false) String tableId,
                                                   @RequestParam(required = false) String experienceId) throws AssociationException {
        log.debug("Requesting all available associations with given filters");
        boolean emptyTableId = !StringUtils.hasText(StringUtils.trimWhitespace(tableId));
        boolean emptyExperienceId = !StringUtils.hasText(StringUtils.trimWhitespace(experienceId));
        if(!emptyTableId || !emptyExperienceId) {
            Optional<String> optTableId = emptyTableId ? Optional.empty() : Optional.of(tableId);
            Optional<String> optExperienceId = emptyExperienceId ? Optional.empty() : Optional.of(experienceId);
            List<AssociationVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optTableId, optExperienceId);
            log.debug("Responding with all available associations with given filters");
            return matchedByFilter;
        }
        log.debug("association filters are empty");
        throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get all Association details by experience id", hidden = true)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Associationes and their details that match the given experience id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = AssociationVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Association id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Associationes available with the given experience id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("experienceid/{experienceId}")
    public List<AssociationVo> getAllAssociationsByExperienceId(@PathVariable String experienceId, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws AssociationException {
        List<AssociationVo> matchedByExperienceIds = new ArrayList<>();
        log.debug("Requesting all available associationes with given experienceId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(experienceId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            matchedByExperienceIds = service.retrieveAllMatchingDetailsByExperienceId(experienceId, Optional.empty());
            log.debug("Responding with all available associationes with given experienceId");
            return matchedByExperienceIds;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(experienceId)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                matchedByExperienceIds = service.retrieveAllMatchingDetailsByExperienceId(experienceId, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing association details with given experienceId having fields cascaded to given level");
                return matchedByExperienceIds;
            } catch (NumberFormatException e) {
                log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_CASCADE_LEVEL_EMPTY.getValue());
                throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug("association experienceId is empty");
        throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "experienceId", experienceId });
    }

    @Operation(summary = "Get Association details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Association that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = AssociationVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Association id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Association found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public AssociationVo getAssociationDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws AssociationException {
        AssociationVo associationDetails = null;
        log.debug("Requesting all details of association by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            associationDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing association details by id");
            return associationDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                associationDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing association details by id wth fields cascaded to given level");
                return associationDetails;
            } catch (NumberFormatException e) {
                log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_CASCADE_LEVEL_EMPTY.getValue());
                throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(AssociationMessageTemplate.MSG_TEMPLATE_ASSOCIATION_ID_EMPTY.getValue());
        throw new AssociationException(BookingErrorCode.BOOKING_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
