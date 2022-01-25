package com.teenthofabud.restaurant.solution.establishmentarea.table.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableException;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableForm;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableMessageTemplate;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableVo;
import com.teenthofabud.restaurant.solution.establishmentarea.table.service.TableService;
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

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("table")
@Slf4j
@Tag(name = "Table API", description = "Manage Table and their details")
public class TableController {

    private static final String MEDIA_TABLE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private TableService service;

    @Autowired
    public void setService(TableService service) {
        this.service = service;
    }

    @Operation(summary = "Create new Table details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Table",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Table attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Table already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Table attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Table",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewTable(@RequestBody(required = false) TableForm form) throws TableException {
        log.debug("Requesting to create new table");
        if(form != null) {
            String id = service.createTable(form);
            log.debug("Responding with identifier of newly created table");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("TableForm is null");
        throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Get all Table details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Tables and their details ordered by table",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = TableVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public List<TableVo> getAllTableNaturallyOrdered() throws TableException {
        log.debug("Requesting all available tables by their natural orders");
        List<TableVo> naturallyOrderedTables = service.retrieveListOfAllTables();
        log.debug("Responding with all available tables by their natural orders");
        return naturallyOrderedTables;
    }

    @Operation(summary = "Get Table details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Table that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = TableVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Gender id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Gender found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public TableVo getTableDetailsById(@PathVariable String id, @RequestParam(required = false)
            @Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws TableException {
        TableVo tableVo = null;
        log.debug("Requesting all details of table by its id");
        if (StringUtils.hasText(StringUtils.trimWhitespace(id)) &&
                StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            tableVo = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing table details by id");
            return tableVo;
        } else if (StringUtils.hasText(StringUtils.trimWhitespace(id)) &&
                StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if (cascadeLevelCode < 0) {
                    throw new NumberFormatException("cascadeUntilLevel can't be zero/negative");
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                tableVo = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing table details by id wth fields cascaded to given level");
                return tableVo;
            } catch (NumberFormatException e) {
                log.debug(TableMessageTemplate.MSG_TEMPLATE_TABLE_CASCADE_LEVEL_EMPTY.getValue());
                throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID,
                        new Object[]{"cascadeUntilLevel", cascadeUntilLevel});
            }
        } else {
            log.debug(TableMessageTemplate.MSG_TEMPLATE_TABLE_ID_EMPTY.getValue());
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID,
                    new Object[]{"id", id});
        }
    }

    @Operation(summary = "Get all Table details by provided criteria i.e. name, description, capacity")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Tables and their details that match the provided criteria",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = TableVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Table search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Tables available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping(value = "filter")
    public List<TableVo> getTableDetailsByCriteria(@RequestParam(required = false) String tableName,
                                                   @RequestParam(required = false) String description,
                                                   @RequestParam(required = false) String capacity) throws TableException {
        log.debug("Requesting all details of table based on filter");
        List<TableVo> tableVo = null;
        boolean isEmptyName = !StringUtils.hasText(StringUtils.trimWhitespace(tableName));
        boolean isEmptyDescription = !StringUtils.hasText(StringUtils.trimWhitespace(description));
        boolean isEmptyCapacity = !StringUtils.hasText(StringUtils.trimWhitespace(capacity));
        if (!isEmptyName || !isEmptyDescription || !isEmptyCapacity) {
            Optional<String> optName = isEmptyName ? Optional.empty() : Optional.of(tableName);
            Optional<String> optDesc = isEmptyDescription ? Optional.empty() : Optional.of(description);
            Optional<String> optCapacity = isEmptyCapacity ? Optional.empty() : Optional.of(capacity);
            tableVo = service.retrieveAllMatchingDetailsByCriteria(optName,optDesc,optCapacity);
            log.debug("Responding with successful retrieval of existing table based on filter");
            return tableVo;
        }
        log.debug("table filters are empty");
        throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get all Table details by floor id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Tables and their details that match the given floor id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = TableVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Table id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Tablees available with the given floor id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("floorid/{floorId}")
    public List<TableVo> getAllTablesByFloorId(@PathVariable String floorId, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws TableException {
        List<TableVo> matchedByFloorIds = new ArrayList<>();
        log.debug("Requesting all available addresses with given floorId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(floorId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            matchedByFloorIds = service.retrieveAllMatchingDetailsByFloorId(floorId, Optional.empty());
            log.debug("Responding with all available addresses with given floorId");
            return matchedByFloorIds;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(floorId)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                matchedByFloorIds = service.retrieveAllMatchingDetailsByFloorId(floorId, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing address details with given floorId having fields cascaded to given level");
                return matchedByFloorIds;
            } catch (NumberFormatException e) {
                log.debug(TableMessageTemplate.MSG_TEMPLATE_TABLE_CASCADE_LEVEL_EMPTY.getValue());
                throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug("address floorId is empty");
        throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "floorId", floorId });
    }

    @Operation(summary = "Update Table details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Table",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Table attribute's value is invalid/Table is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Table found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Table already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Table details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void updateExistingTable(@PathVariable String id, @RequestBody(required = false) TableForm form) throws TableException {
        log.debug("Requesting to update all attributes of existing table");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateTable(id, form);
                log.debug("Responding with successful updation of attributes for existing table");
                return;
            }
            log.debug("TableForm is null");
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(TableMessageTemplate.MSG_TEMPLATE_TABLE_ID_EMPTY.getValue());
        throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Table by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Table",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Table id is invalid/Table is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Table found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Table attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Table",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingTable(@PathVariable String id) throws TableException {
        log.debug("Requesting to soft delete table");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteTable(id);
            log.debug("Responding with successful deletion of existing table");
            return;
        }
        log.debug(TableMessageTemplate.MSG_TEMPLATE_TABLE_ID_EMPTY.getValue());
        throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Table attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Table with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Table attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Table found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Table attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Table with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_TABLE_APPLICATION_JSON_PATCH)
    public void patchExistingTable(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws TableException {
        log.debug("Requesting to patch of table attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnTable(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing table");
                return;
            }
            log.debug("table patch document is null");
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(TableMessageTemplate.MSG_TEMPLATE_TABLE_ID_EMPTY.getValue());
        throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
