package com.teenthofabud.restaurant.solution.establishmentarea.floor.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorForm;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorMessageTemplate;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.service.FloorService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
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

import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("floor")
@Slf4j
@Tag(name = "Floor API", description = "Manage Floor and their details")
public class FloorController {

    private static final String MEDIA_FLOOR_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private FloorService service;

    @Autowired
    public void setService(FloorService service) {
        this.service = service;
    }

    @Operation(summary = "Create new Floor details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Floor",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Floor attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Floor already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Floor attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Floor",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewFloor(@RequestBody(required = false) FloorForm form) throws FloorException {
        log.debug("Requesting to create new floor");
        if(form != null) {
            String id = service.createFloor(form);
            log.debug("Responding with identifier of newly created floor");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("FloorForm is null");
        throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Get all Floor details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Floors and their details ordered by floor",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = FloorVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public List<FloorVo> getAllFloorNaturallyOrdered() {
        log.debug("Requesting all available floors by their natural orders");
        List<FloorVo> naturallyOrderedFloors = service.retrieveListOfAllFloors();
        log.debug("Responding with all available accounts by their natural orders");
        return naturallyOrderedFloors;
    }

    @Operation(summary = "Get Floor details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Floor that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = FloorVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Gender id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Gender found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public FloorVo getFloorDetailsById(@PathVariable String id, @RequestParam(required = false)
            @Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws FloorException {
        FloorVo floorVo = null;
        log.debug("Requesting all details of floor by its id");
        if (StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            floorVo = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing floor details by id");
            return floorVo;
        } else if (StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if (cascadeLevelCode < 0) {
                    throw new NumberFormatException("cascadeUntilLevel can't be zero/negative");
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                floorVo = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing floor details by id wth fields cascaded to given level");
                return floorVo;
            } catch (NumberFormatException e) {
                log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_CASCADE_LEVEL_EMPTY.getValue());
                throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID,
                        new Object[]{"cascadeUntilLevel", cascadeUntilLevel});
            }
        } else {
            log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_ID_EMPTY.getValue());
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID,
                    new Object[]{"id", id});
        }
    }

    @Operation(summary = "Get all Floor details by provided criteria")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Floors and their details that match the provided criteria",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = FloorVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Floor search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Floors available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping(value = "/criteria/{name}")
    public List<FloorVo> getFloorDetailsByCriteria(@PathVariable String name) throws FloorException {
        log.debug("Requesting all details of floor based on filter");
        List<FloorVo> floorVo = null;
        boolean isEmptyName = !StringUtils.hasText(StringUtils.trimWhitespace(name));
        if (!isEmptyName) {
            floorVo = service.retrieveAllMatchingDetailsByCriteria(Optional.of(name));
            log.debug("Responding with successful retrieval of existing floor based on filter");
            return floorVo;
        }
        log.debug("floor filters are empty");
        throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Update Floor details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Floor",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Floor attribute's value is invalid/Floor is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Floor found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Floor already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Floor details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.ACCEPTED)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void updateExistingFloor(@PathVariable String id, @RequestBody(required = false) FloorForm form) throws FloorException {
        log.debug("Requesting to update all attributes of existing floor");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateFloor(id, form);
                log.debug("Responding with successful updation of attributes for existing account");
                return;
            }
            log.debug("FloorForm is null");
            throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_ID_EMPTY.getValue());
        throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Floor by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Floor",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Floor id is invalid/Floor is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Floor found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Floor attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Floor",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingFloor(@PathVariable String id) throws FloorException {
        log.debug("Requesting to soft delete account");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteFloor(id);
            log.debug("Responding with successful deletion of existing account");
            return;
        }
        log.debug(FloorMessageTemplate.MSG_TEMPLATE_FLOOR_ID_EMPTY.getValue());
        throw new FloorException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
