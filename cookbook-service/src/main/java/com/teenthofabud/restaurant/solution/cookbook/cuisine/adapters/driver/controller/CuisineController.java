package com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driver.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineException;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.dto.CuisineRequest;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineMessageTemplate;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.dto.CuisineResponse;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.service.CuisineService;
import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
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

import java.util.List;
import java.util.Optional;
import java.util.Set;

@RestController
@RequestMapping("cuisine")
@Slf4j
@Tag(name = "Cuisine API", description = "Manage Cuisines and their details")
public class CuisineController {

    private static final String MEDIA_COOK_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(CuisineService service) {
        this.service = service;
    }

    private CuisineService service;

    @Operation(summary = "Create new Cuisine details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Cuisine",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Cuisine attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Cuisine already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Cuisine attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Cuisine",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewCuisine(@RequestBody(required = false) CuisineRequest form) throws CuisineException {
        log.debug("Requesting to create new cuisine");
        if(form != null) {
            String id = service.createCuisine(form);
            log.debug("Responding with identifier of newly created new cuisine");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("CuisineRequest is null");
        throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Cuisine details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Cuisine",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Cuisine attribute's value is invalid/Cuisine is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Cuisine found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Cuisine already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Cuisine details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingCuisine(@PathVariable String id, @RequestBody(required = false) CuisineRequest form) throws CuisineException {
        log.debug("Requesting to update all attributes of existing cuisine");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateCuisine(id, form);
                log.debug("Responding with successful updation of attributes for existing cuisine");
                return;
            }
            log.debug("CuisineRequest is null");
            throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_ID_EMPTY.getValue());
        throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Cuisine by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Cuisine",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Cuisine id is invalid/Cuisine is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Cuisine found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Cuisine attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Cuisine",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingCuisine(@PathVariable String id) throws CuisineException {
        log.debug("Requesting to soft delete cuisine");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteCuisine(id);
            log.debug("Responding with successful deletion of existing cuisine");
            return;
        }
        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_ID_EMPTY.getValue());
        throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Cuisine attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Cuisine with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Cuisine attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Cuisine found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Cuisine attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Cuisine with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_COOK_APPLICATION_JSON_PATCH)
    public void patchExistingCuisine(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws CuisineException {
        log.debug("Requesting to patch of cuisine attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnCuisine(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing cuisine");
                return;
            }
            log.debug("cuisine patch document is null");
            throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_ID_EMPTY.getValue());
        throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Cuisine details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Cuisines and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = CuisineResponse.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<CuisineResponse> getAllCuisineNaturallyOrdered() {
        log.debug("Requesting all available cuisines by their natural orders");
        Set<CuisineResponse> naturallyOrderedCuisines = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available cuisines by their natural orders");
        return naturallyOrderedCuisines;
    }

    @Operation(summary = "Get all Cuisine details by name, description")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Cuisines and their details that match the provided name, description",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = CuisineResponse.class))) }),
            @ApiResponse(responseCode = "400", description = "Cuisine search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Cuisines available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<CuisineResponse> getAllCuisinesByFilters(@RequestParam(required = false) String name, @RequestParam(required = false) String description)
            throws CuisineException {
        log.debug("Requesting all available cuisines with given filters");
        boolean emptyName = !StringUtils.hasText(StringUtils.trimWhitespace(name));
        boolean emptyDescription = !StringUtils.hasText(StringUtils.trimWhitespace(description));
        if(!emptyName || !emptyDescription) {
            Optional<String> optCuisineName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optDescription = emptyDescription ? Optional.empty() : Optional.of(description);
            List<CuisineResponse> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optCuisineName, optDescription);
            log.debug("Responding with all available cuisines with given filters");
            return matchedByFilter;
        }
        log.debug("cuisine filters are empty");
        throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get Cuisine details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Cuisine that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CuisineResponse.class)) }),
            @ApiResponse(responseCode = "400", description = "Cuisine id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Cuisine found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public CuisineResponse getCuisineDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws CuisineException {
        CuisineResponse cuisineDetails = null;
        log.debug("Requesting all details of cuisine by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            cuisineDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing cuisine details by id");
            return cuisineDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                cuisineDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing cuisine details by id wth fields cascaded to given level");
                return cuisineDetails;
            } catch (NumberFormatException e) {
                log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_CASCADE_LEVEL_EMPTY.getValue());
                throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_ID_EMPTY.getValue());
        throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
