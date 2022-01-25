package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenException;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenForm;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenMessageTemplate;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenVo;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.service.KitchenService;
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
@RequestMapping("kitchen")
@Slf4j
@Tag(name = "Kitchen API", description = "Manage Kitchen and their details")
public class KitchenController {

    private static final String MEDIA_KITCHEN_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private KitchenService service;

    @Autowired
    public void setService(KitchenService service) {
        this.service = service;
    }

    @Operation(summary = "Create new Kitchen details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Kitchen",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Kitchen attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Kitchen already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Kitchen attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Kitchen",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewKitchen(@RequestBody(required = false) KitchenForm form) throws KitchenException {
        log.debug("Requesting to create new kitchen");
        if(form != null) {
            String id = service.createKitchen(form);
            log.debug("Responding with identifier of newly created kitchen");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("KitchenForm is null");
        throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Get all Kitchen details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Kitchens and their details ordered by kitchen",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = KitchenVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public List<KitchenVo> getAllKitchenNaturallyOrdered() throws KitchenException {
        log.debug("Requesting all available kitchens by their natural orders");
        List<KitchenVo> naturallyOrderedKitchens = service.retrieveListOfAllKitchens();
        log.debug("Responding with all available floors by their natural orders");
        return naturallyOrderedKitchens;
    }

    @Operation(summary = "Get Kitchen details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Kitchen that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = KitchenVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Gender id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Gender found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public KitchenVo getKitchenDetailsById(@PathVariable String id, @RequestParam(required = false)
            @Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws KitchenException {
        KitchenVo kitchenVo = null;
        log.debug("Requesting all details of kitchen by its id");
        if (StringUtils.hasText(StringUtils.trimWhitespace(id)) &&
                StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            kitchenVo = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing kitchen details by id");
            return kitchenVo;
        } else if (StringUtils.hasText(StringUtils.trimWhitespace(id)) &&
                StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if (cascadeLevelCode < 0) {
                    throw new NumberFormatException("cascadeUntilLevel can't be zero/negative");
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                kitchenVo = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing kitchen details by id wth fields cascaded to given level");
                return kitchenVo;
            } catch (NumberFormatException e) {
                log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_CASCADE_LEVEL_EMPTY.getValue());
                throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID,
                        new Object[]{"cascadeUntilLevel", cascadeUntilLevel});
            }
        } else {
            log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_ID_EMPTY.getValue());
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID,
                    new Object[]{"id", id});
        }
    }

    @Operation(summary = "Get all Kitchen details by provided criteria i.e. name, description")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Kitchens and their details that match the provided criteria",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = KitchenVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Kitchen search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Kitchens available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<KitchenVo> getKitchenDetailsByFilter(@RequestParam(required = false) String kitchenName,
                                                     @RequestParam(required = false) String description) throws KitchenException {
        log.debug("Requesting all details of kitchen based on filter");
        List<KitchenVo> kitchenVo = null;
        boolean isEmptyName = !StringUtils.hasText(StringUtils.trimWhitespace(kitchenName));
        boolean isEmptyDescription = !StringUtils.hasText(StringUtils.trimWhitespace(description));

        if (!isEmptyName || !isEmptyDescription) {
            Optional<String> optName = isEmptyName ? Optional.empty() : Optional.of(kitchenName);
            Optional<String> optDesc = isEmptyDescription ? Optional.empty() : Optional.of(description);
            kitchenVo = service.retrieveAllMatchingDetailsByCriteria(optName,optDesc);
            log.debug("Responding with successful retrieval of existing kitchen based on filter");
            return kitchenVo;
        }
        log.debug("kitchen filters are empty");
        throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get all Kitchen details by floor id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Kitchens and their details that match the given floor id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = KitchenVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Kitchen id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Kitchens available with the given floor id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("floorid/{floorId}")
    public List<KitchenVo> getAllKitchensByFloorId(@PathVariable String floorId, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws KitchenException {
        List<KitchenVo> matchedByFloorIds = new ArrayList<>();
        log.debug("Requesting all available kitchens with given floorId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(floorId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            matchedByFloorIds = service.retrieveAllMatchingDetailsByFloorId(floorId, Optional.empty());
            log.debug("Responding with all available kitchens with given floorId");
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
                log.debug("Responding with successful retrieval of existing kitchen details with given floorId having fields cascaded to given level");
                return matchedByFloorIds;
            } catch (NumberFormatException e) {
                log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_CASCADE_LEVEL_EMPTY.getValue());
                throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug("kitchen floorId is empty");
        throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "floorId", floorId });
    }

    @Operation(summary = "Update Kitchen details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Kitchen",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Kitchen attribute's value is invalid/Kitchen is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Kitchen found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Kitchen already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Kitchen details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void updateExistingKitchen(@PathVariable String id, @RequestBody(required = false) KitchenForm form) throws KitchenException {
        log.debug("Requesting to update all attributes of existing kitchen");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateKitchen(id, form);
                log.debug("Responding with successful updation of attributes for existing floor");
                return;
            }
            log.debug("KitchenForm is null");
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_ID_EMPTY.getValue());
        throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Kitchen by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Kitchen",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Kitchen id is invalid/Kitchen is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Kitchen found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Kitchen attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Kitchen",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingKitchen(@PathVariable String id) throws KitchenException {
        log.debug("Requesting to soft delete floor");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteKitchen(id);
            log.debug("Responding with successful deletion of existing floor");
            return;
        }
        log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_ID_EMPTY.getValue());
        throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Kitchen attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Kitchen with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Kitchen attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Kitchen found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Kitchen attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Kitchen with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_KITCHEN_APPLICATION_JSON_PATCH)
    public void patchExistingKitchen(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws KitchenException {
        log.debug("Requesting to patch of kitchen attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnKitchen(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing kitchen");
                return;
            }
            log.debug("kitchen patch document is null");
            throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(KitchenMessageTemplate.MSG_TEMPLATE_KITCHEN_ID_EMPTY.getValue());
        throw new KitchenException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }
}
