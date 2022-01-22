package com.teenthofabud.restaurant.solution.settings.discount.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountException;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountForm;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountMessageTemplate;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountVo;
import com.teenthofabud.restaurant.solution.settings.discount.service.DiscountService;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
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
@RequestMapping("discount")
@Slf4j
@Tag(name = "Discount API", description = "Manage Discounts and their details")
public class DiscountController {

    private static final String MEDIA_SETTINGS_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(DiscountService service) {
        this.service = service;
    }

    private DiscountService service;

    @Operation(summary = "Create new Discount details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Discount",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Discount attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Discount already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Discount attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Discount",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewDiscount(@RequestBody(required = false) DiscountForm form) throws DiscountException {
        log.debug("Requesting to create new discount");
        if(form != null) {
            String id = service.createDiscount(form);
            log.debug("Responding with identifier of newly created new discount");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("DiscountForm is null");
        throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Discount details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Discount",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Discount attribute's value is invalid/Discount is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Discount found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Discount already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Discount details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingDiscount(@PathVariable String id, @RequestBody(required = false) DiscountForm form) throws DiscountException {
        log.debug("Requesting to update all attributes of existing discount");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateDiscount(id, form);
                log.debug("Responding with successful updation of attributes for existing discount");
                return;
            }
            log.debug("DiscountForm is null");
            throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_ID_EMPTY.getValue());
        throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Discount by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Discount",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Discount id is invalid/Discount is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Discount found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Discount attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Discount",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingDiscount(@PathVariable String id) throws DiscountException {
        log.debug("Requesting to soft delete discount");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteDiscount(id);
            log.debug("Responding with successful deletion of existing discount");
            return;
        }
        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_ID_EMPTY.getValue());
        throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Discount attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Discount with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Discount attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Discount found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Discount attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Discount with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_SETTINGS_APPLICATION_JSON_PATCH)
    public void patchExistingDiscount(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws DiscountException {
        log.debug("Requesting to patch of discount attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnDiscount(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing discount");
                return;
            }
            log.debug("discount patch document is null");
            throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_ID_EMPTY.getValue());
        throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Discount details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Discounts and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = DiscountVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<DiscountVo> getAllDiscountNaturallyOrdered() {
        log.debug("Requesting all available discounts by their natural orders");
        Set<DiscountVo> naturallyOrderedDiscounts = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available discounts by their natural orders");
        return naturallyOrderedDiscounts;
    }

    @Operation(summary = "Get all Discount details by name, description")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Discounts and their details that match the provided name, description",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = DiscountVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Discount search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Discounts available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<DiscountVo> getAllDiscountsByFilters(@RequestParam(required = false) String name,
                                                   @RequestParam(required = false) String description) throws DiscountException {
        log.debug("Requesting all available discounts with given filters");
        boolean emptyName = !StringUtils.hasText(StringUtils.trimWhitespace(name));
        boolean emptyDescription = !StringUtils.hasText(StringUtils.trimWhitespace(description));
        if(!emptyName || !emptyDescription) {
            Optional<String> optName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optDescription = emptyDescription ? Optional.empty() : Optional.of(description);
            List<DiscountVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optName, optDescription);
            log.debug("Responding with all available discounts with given filters");
            return matchedByFilter;
        }
        log.debug("discount filters are empty");
        throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get Discount details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Discount that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = DiscountVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Discount id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Discount found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public DiscountVo getDiscountDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws DiscountException {
        DiscountVo discountDetails = null;
        log.debug("Requesting all details of discount by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            discountDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing discount details by id");
            return discountDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                discountDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing discount details by id wth fields cascaded to given level");
                return discountDetails;
            } catch (NumberFormatException e) {
                log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_CASCADE_LEVEL_EMPTY.getValue());
                throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(DiscountMessageTemplate.MSG_TEMPLATE_DISCOUNT_ID_EMPTY.getValue());
        throw new DiscountException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
