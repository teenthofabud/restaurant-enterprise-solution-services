package com.teenthofabud.restaurant.solution.settings.deliverypartner.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerException;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerForm;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerMessageTemplate;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerVo;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.service.DeliveryPartnerService;
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
@RequestMapping("deliverypartner")
@Slf4j
@Tag(name = "DeliveryPartner API", description = "Manage Delivery Partners and their details")
public class DeliveryPartnerController {

    private static final String MEDIA_SETTINGS_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(DeliveryPartnerService service) {
        this.service = service;
    }

    private DeliveryPartnerService service;

    @Operation(summary = "Create new DeliveryPartner details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created DeliveryPartner",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "DeliveryPartner attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "DeliveryPartner already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No DeliveryPartner attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new DeliveryPartner",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewDeliveryPartner(@RequestBody(required = false) DeliveryPartnerForm form) throws DeliveryPartnerException {
        log.debug("Requesting to create new deliveryPartner");
        if(form != null) {
            String id = service.createDeliveryPartner(form);
            log.debug("Responding with identifier of newly created new deliveryPartner");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("DeliveryPartnerForm is null");
        throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update DeliveryPartner details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of DeliveryPartner",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "DeliveryPartner attribute's value is invalid/DeliveryPartner is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No DeliveryPartner found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "DeliveryPartner already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update DeliveryPartner details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingDeliveryPartner(@PathVariable String id, @RequestBody(required = false) DeliveryPartnerForm form) throws DeliveryPartnerException {
        log.debug("Requesting to update all attributes of existing deliveryPartner");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateDeliveryPartner(id, form);
                log.debug("Responding with successful updation of attributes for existing deliveryPartner");
                return;
            }
            log.debug("DeliveryPartnerForm is null");
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_DELIVERY_PARTNER_ID_EMPTY.getValue());
        throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete DeliveryPartner by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted DeliveryPartner",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "DeliveryPartner id is invalid/DeliveryPartner is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No DeliveryPartner found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No DeliveryPartner attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete DeliveryPartner",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingDeliveryPartner(@PathVariable String id) throws DeliveryPartnerException {
        log.debug("Requesting to soft delete deliveryPartner");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteDeliveryPartner(id);
            log.debug("Responding with successful deletion of existing deliveryPartner");
            return;
        }
        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_DELIVERY_PARTNER_ID_EMPTY.getValue());
        throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch DeliveryPartner attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of DeliveryPartner with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "DeliveryPartner attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No DeliveryPartner found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No DeliveryPartner attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of DeliveryPartner with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_SETTINGS_APPLICATION_JSON_PATCH)
    public void patchExistingDeliveryPartner(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws DeliveryPartnerException {
        log.debug("Requesting to patch of deliveryPartner attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnDeliveryPartner(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing deliveryPartner");
                return;
            }
            log.debug("deliveryPartner patch document is null");
            throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_DELIVERY_PARTNER_ID_EMPTY.getValue());
        throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all DeliveryPartner details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Delivery Partners and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = DeliveryPartnerVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<DeliveryPartnerVo> getAllDeliveryPartnerNaturallyOrdered() {
        log.debug("Requesting all available delivery partners by their natural orders");
        Set<DeliveryPartnerVo> naturallyOrderedDeliveryPartners = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available delivery partners by their natural orders");
        return naturallyOrderedDeliveryPartners;
    }

    @Operation(summary = "Get all DeliveryPartner details by name, description")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Delivery Partners and their details that match the provided name, description",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = DeliveryPartnerVo.class))) }),
            @ApiResponse(responseCode = "400", description = "DeliveryPartner search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Delivery Partners available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<DeliveryPartnerVo> getAllDeliveryPartnersByFilters(@RequestParam(required = false) String name,
                                                                 @RequestParam(required = false) String description) throws DeliveryPartnerException {
        log.debug("Requesting all available delivery partners with given filters");
        boolean emptyName = !StringUtils.hasText(StringUtils.trimWhitespace(name));
        boolean emptyDescription = !StringUtils.hasText(StringUtils.trimWhitespace(description));
        if(!emptyName || !emptyDescription) {
            Optional<String> optName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optDescription = emptyDescription ? Optional.empty() : Optional.of(description);
            List<DeliveryPartnerVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optName, optDescription);
            log.debug("Responding with all available delivery partners with given filters");
            return matchedByFilter;
        }
        log.debug("deliveryPartner filters are empty");
        throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get DeliveryPartner details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of DeliveryPartner that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = DeliveryPartnerVo.class)) }),
            @ApiResponse(responseCode = "400", description = "DeliveryPartner id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No DeliveryPartner found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public DeliveryPartnerVo getDeliveryPartnerDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws DeliveryPartnerException {
        DeliveryPartnerVo deliveryPartnerDetails = null;
        log.debug("Requesting all details of deliveryPartner by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            deliveryPartnerDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing deliveryPartner details by id");
            return deliveryPartnerDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                deliveryPartnerDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing deliveryPartner details by id wth fields cascaded to given level");
                return deliveryPartnerDetails;
            } catch (NumberFormatException e) {
                log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_DELIVERY_PARTNER_CASCADE_LEVEL_EMPTY.getValue());
                throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(DeliveryPartnerMessageTemplate.MSG_TEMPLATE_DELIVERY_PARTNER_ID_EMPTY.getValue());
        throw new DeliveryPartnerException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
