package com.teenthofabud.restaurant.solution.settings.paymentmethod.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodException;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodForm;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodMessageTemplate;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodVo;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.service.PaymentMethodService;
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
@RequestMapping("paymentmethod")
@Slf4j
@Tag(name = "PaymentMethod API", description = "Manage Payment Methods and their details")
public class PaymentMethodController {

    private static final String MEDIA_SETTINGS_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(PaymentMethodService service) {
        this.service = service;
    }

    private PaymentMethodService service;

    @Operation(summary = "Create new PaymentMethod details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created PaymentMethod",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "PaymentMethod attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "PaymentMethod already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No PaymentMethod attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new PaymentMethod",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewPaymentMethod(@RequestBody(required = false) PaymentMethodForm form) throws PaymentMethodException {
        log.debug("Requesting to create new paymentMethod");
        if(form != null) {
            String id = service.createPaymentMethod(form);
            log.debug("Responding with identifier of newly created new paymentMethod");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("DiscountForm is null");
        throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update PaymentMethod details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of PaymentMethod",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "PaymentMethod attribute's value is invalid/PaymentMethod is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No PaymentMethod found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "PaymentMethod already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update PaymentMethod details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingPaymentMethod(@PathVariable String id, @RequestBody(required = false) PaymentMethodForm form) throws PaymentMethodException {
        log.debug("Requesting to update all attributes of existing paymentMethod");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updatePaymentMethod(id, form);
                log.debug("Responding with successful updation of attributes for existing paymentMethod");
                return;
            }
            log.debug("DiscountForm is null");
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_ID_EMPTY.getValue());
        throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete PaymentMethod by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted PaymentMethod",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "PaymentMethod id is invalid/PaymentMethod is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No PaymentMethod found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No PaymentMethod attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete PaymentMethod",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingPaymentMethod(@PathVariable String id) throws PaymentMethodException {
        log.debug("Requesting to soft delete paymentMethod");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deletePaymentMethod(id);
            log.debug("Responding with successful deletion of existing paymentMethod");
            return;
        }
        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_ID_EMPTY.getValue());
        throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch PaymentMethod attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of PaymentMethod with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "PaymentMethod attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No PaymentMethod found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No PaymentMethod attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of PaymentMethod with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_SETTINGS_APPLICATION_JSON_PATCH)
    public void patchExistingPaymentMethod(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws PaymentMethodException {
        log.debug("Requesting to patch of paymentMethod attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnPaymentMethod(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing paymentMethod");
                return;
            }
            log.debug("paymentMethod patch document is null");
            throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_ID_EMPTY.getValue());
        throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all PaymentMethod details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Payment Methods and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = PaymentMethodVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<PaymentMethodVo> getAllPaymentMethodNaturallyOrdered() {
        log.debug("Requesting all available payment methods by their natural orders");
        Set<PaymentMethodVo> naturallyOrderedPaymentMethods = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available payment methods by their natural orders");
        return naturallyOrderedPaymentMethods;
    }

    @Operation(summary = "Get all PaymentMethod details by name, description")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Payment Methods and their details that match the provided name, description",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = PaymentMethodVo.class))) }),
            @ApiResponse(responseCode = "400", description = "PaymentMethod search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Payment Methods available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<PaymentMethodVo> getAllPaymentMethodsByFilters(@RequestParam(required = false) String name,
                                                                @RequestParam(required = false) String description) throws PaymentMethodException {
        log.debug("Requesting all available payment methods with given filters");
        boolean emptyName = !StringUtils.hasText(StringUtils.trimWhitespace(name));
        boolean emptyDescription = !StringUtils.hasText(StringUtils.trimWhitespace(description));
        if(!emptyName || !emptyDescription) {
            Optional<String> optName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optDescription = emptyDescription ? Optional.empty() : Optional.of(description);
            List<PaymentMethodVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optName, optDescription);
            log.debug("Responding with all available payment methods with given filters");
            return matchedByFilter;
        }
        log.debug("paymentMethod filters are empty");
        throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get PaymentMethod details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of PaymentMethod that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = PaymentMethodVo.class)) }),
            @ApiResponse(responseCode = "400", description = "PaymentMethod id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No PaymentMethod found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public PaymentMethodVo getPaymentMethodDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws PaymentMethodException {
        PaymentMethodVo paymentMethodDetails = null;
        log.debug("Requesting all details of paymentMethod by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            paymentMethodDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing paymentMethod details by id");
            return paymentMethodDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                paymentMethodDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing paymentMethod details by id wth fields cascaded to given level");
                return paymentMethodDetails;
            } catch (NumberFormatException e) {
                log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_CASCADE_LEVEL_EMPTY.getValue());
                throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(PaymentMethodMessageTemplate.MSG_TEMPLATE_PAYMENT_METHOD_ID_EMPTY.getValue());
        throw new PaymentMethodException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
