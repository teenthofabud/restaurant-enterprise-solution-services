package com.teenthofabud.restaurant.solution.inventory.quantity.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.inventory.error.InventoryErrorCode;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityException;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityForm;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityMessageTemplate;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityVo;
import com.teenthofabud.restaurant.solution.inventory.quantity.service.QuantityService;
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
@RequestMapping("quantity")
@Slf4j
@Tag(name = "Quantity API", description = "Manage Quantities and their details")
public class QuantityController {

    private static final String MEDIA_INVENTORY_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(QuantityService service) {
        this.service = service;
    }

    private QuantityService service;

    @Operation(summary = "Create new Quantity details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Quantity",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Quantity attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Quantity already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Quantity attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Quantity",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewQuantity(@RequestBody(required = false) QuantityForm form) throws QuantityException {
        log.debug("Requesting to create new quantity");
        if(form != null) {
            String id = service.createQuantity(form);
            log.debug("Responding with identifier of newly created new quantity");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("QuantityForm is null");
        throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Quantity details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Quantity",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Quantity attribute's value is invalid/Quantity is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Quantity found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Quantity already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Quantity details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingQuantity(@PathVariable String id, @RequestBody(required = false) QuantityForm form) throws QuantityException {
        log.debug("Requesting to update all attributes of existing quantity");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateQuantity(id, form);
                log.debug("Responding with successful updation of attributes for existing quantity");
                return;
            }
            log.debug("QuantityForm is null");
            throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(QuantityMessageTemplate.MSG_TEMPLATE_QUANTITY_ID_EMPTY.getValue());
        throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Quantity by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Quantity",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Quantity id is invalid/Quantity is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Quantity found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Quantity attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Quantity",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingQuantity(@PathVariable String id) throws QuantityException {
        log.debug("Requesting to soft delete quantity");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteQuantity(id);
            log.debug("Responding with successful deletion of existing quantity");
            return;
        }
        log.debug(QuantityMessageTemplate.MSG_TEMPLATE_QUANTITY_ID_EMPTY.getValue());
        throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Quantity attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Quantity with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Quantity attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Quantity found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Quantity attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Quantity with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_INVENTORY_APPLICATION_JSON_PATCH)
    public void patchExistingQuantity(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws QuantityException {
        log.debug("Requesting to patch of quantity attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnQuantity(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing quantity");
                return;
            }
            log.debug("quantity patch document is null");
            throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(QuantityMessageTemplate.MSG_TEMPLATE_QUANTITY_ID_EMPTY.getValue());
        throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Quantity details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Quantities and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = QuantityVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<QuantityVo> getAllQuantityNaturallyOrdered() {
        log.debug("Requesting all available quantities by their natural orders");
        Set<QuantityVo> naturallyOrderedQuantities = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available quantities by their natural orders");
        return naturallyOrderedQuantities;
    }

    @Operation(summary = "Get all Quantity details by product id", hidden = true)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Quantityes and their details that match the given product id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = QuantityVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Quantity id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Quantityes available with the given product id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("productid/{productId}")
    public List<QuantityVo> getAllQuantitiesByProductId(@PathVariable String productId, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws QuantityException {
        List<QuantityVo> matchedByProductIds = new ArrayList<>();
        log.debug("Requesting all available addresses with given productId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(productId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            matchedByProductIds = service.retrieveAllMatchingDetailsByProductId(productId, Optional.empty());
            log.debug("Responding with all available addresses with given productId");
            return matchedByProductIds;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(productId)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                matchedByProductIds = service.retrieveAllMatchingDetailsByProductId(productId, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing address details with given productId having fields cascaded to given level");
                return matchedByProductIds;
            } catch (NumberFormatException e) {
                log.debug(QuantityMessageTemplate.MSG_TEMPLATE_QUANTITY_CASCADE_LEVEL_EMPTY.getValue());
                throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug("address productId is empty");
        throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "productId", productId });
    }

    @Operation(summary = "Get all Quantity details by name, description")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Quantities and their details that match the provided name, description",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = QuantityVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Quantity search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Quantities available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<QuantityVo> getAllQuantitiesByFilters(@RequestParam(required = false) String productId,
                                                   @RequestParam(required = false) String weightId) throws QuantityException {
        log.debug("Requesting all available quantities with given filters");
        boolean emptyProductId = !StringUtils.hasText(StringUtils.trimWhitespace(productId));
        boolean emptyWeightId = !StringUtils.hasText(StringUtils.trimWhitespace(weightId));
        if(!emptyProductId || !emptyWeightId) {
            Optional<String> optProductId = emptyProductId ? Optional.empty() : Optional.of(productId);
            Optional<String> optWeightId = emptyWeightId ? Optional.empty() : Optional.of(weightId);
            List<QuantityVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optProductId, optWeightId);
            log.debug("Responding with all available quantities with given filters");
            return matchedByFilter;
        }
        log.debug("quantity filters are empty");
        throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get Quantity details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Quantity that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = QuantityVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Quantity id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Quantity found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public QuantityVo getQuantityDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws QuantityException {
        QuantityVo quantityDetails = null;
        log.debug("Requesting all details of quantity by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            quantityDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing quantity details by id");
            return quantityDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                quantityDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing quantity details by id wth fields cascaded to given level");
                return quantityDetails;
            } catch (NumberFormatException e) {
                log.debug(QuantityMessageTemplate.MSG_TEMPLATE_QUANTITY_CASCADE_LEVEL_EMPTY.getValue());
                throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(QuantityMessageTemplate.MSG_TEMPLATE_QUANTITY_ID_EMPTY.getValue());
        throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
