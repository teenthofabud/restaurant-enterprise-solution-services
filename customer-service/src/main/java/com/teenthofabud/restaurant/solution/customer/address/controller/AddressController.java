package com.teenthofabud.restaurant.solution.customer.address.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressException;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressForm;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressMessageTemplate;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressVo;
import com.teenthofabud.restaurant.solution.customer.address.service.AddressService;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
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
@RequestMapping("address")
@Slf4j
@Tag(name = "Address API", description = "Manage Addresss and their details")
public class AddressController {

    private static final String MEDIA_CUST_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(AddressService service) {
        this.service = service;
    }

    private AddressService service;

    @Operation(summary = "Create new Address details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Address",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Address attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Address already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Address attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Address",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewAddress(@RequestBody(required = false) AddressForm form) throws AddressException {
        log.debug("Requesting to create new address");
        if(form != null) {
            String id = service.createAddress(form);
            log.debug("Responding with identifier of newly created new address");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("AddressForm is null");
        throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Address details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Address",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Address attribute's value is invalid/Address is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Address found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Address already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Address details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingAddress(@PathVariable String id, @RequestBody(required = false) AddressForm form) throws AddressException {
        log.debug("Requesting to update all attributes of existing address");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateAddress(id, form);
                log.debug("Responding with successful updation of attributes for existing address");
                return;
            }
            log.debug("AddressForm is null");
            throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_ID_EMPTY.getValue());
        throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Address by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Address",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Address id is invalid/Address is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Address found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Address attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Address",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingAddress(@PathVariable String id) throws AddressException {
        log.debug("Requesting to soft delete address");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteAddress(id);
            log.debug("Responding with successful deletion of existing address");
            return;
        }
        log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_ID_EMPTY.getValue());
        throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Address attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Address with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Address attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Address found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Address attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Address with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_CUST_APPLICATION_JSON_PATCH)
    public void patchExistingAddress(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws AddressException {
        log.debug("Requesting to patch of address attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnAddress(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing address");
                return;
            }
            log.debug("address patch document is null");
            throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_ID_EMPTY.getValue());
        throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Address details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Addresss and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = AddressVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<AddressVo> getAllAddressNaturallyOrdered() {
        log.debug("Requesting all available addresses by their natural orders");
        Set<AddressVo> naturallyOrderedAddresss = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available addresses by their natural orders");
        return naturallyOrderedAddresss;
    }

    @Operation(summary = "Get all Address details by account id", hidden = true)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Addresses and their details that match the given account id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = AddressVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Address id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Addresses available with the given account id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("accountid/{accountId}")
    public List<AddressVo> getAllAddresssByAccountId(@PathVariable String accountId, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws AddressException {
        List<AddressVo> matchedByAccountIds = new ArrayList<>();
        log.debug("Requesting all available addresses with given accountId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(accountId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            matchedByAccountIds = service.retrieveAllMatchingDetailsByAccountId(accountId, Optional.empty());
            log.debug("Responding with all available addresses with given accountId");
            return matchedByAccountIds;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(accountId)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                matchedByAccountIds = service.retrieveAllMatchingDetailsByAccountId(accountId, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing address details with given accountId having fields cascaded to given level");
                return matchedByAccountIds;
            } catch (NumberFormatException e) {
                log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_CASCADE_LEVEL_EMPTY.getValue());
                throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug("address accountId is empty");
        throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "accountId", accountId });
    }

    @Operation(summary = "Get all Address details by name, address line 1, address line 2, pincode, country id, city id, state id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Addresses and their details that match the provided name, address line 1, address line 2, pincode, country id, city id, state id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = AddressVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Address search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Addresses available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<AddressVo> getAllAddressesByFilters(@RequestParam(required = false) String name, @RequestParam(required = false) String pincode,
                                                   @RequestParam(required = false) String countryId, @RequestParam(required = false) String cityId,
                                                   @RequestParam(required = false) String stateId, @RequestParam(required = false) String addressLine1,
                                                   @RequestParam(required = false) String addressLine2) throws AddressException {
        log.debug("Requesting all available addresses with given filters");
        boolean emptyName = StringUtils.isEmpty(StringUtils.trimWhitespace(name));
        log.debug("filter name is provided: {} as ", emptyName, name);
        boolean emptyAddressLine1 = StringUtils.isEmpty(StringUtils.trimWhitespace(addressLine1));
        log.debug("filter addressLine1 name is provided: {} as ", emptyAddressLine1, addressLine1);
        boolean emptyAddressLine2 = StringUtils.isEmpty(StringUtils.trimWhitespace(addressLine2));
        log.debug("filter addressLine2 is provided: {} as ", emptyAddressLine2, addressLine2);
        boolean emptyPincode = StringUtils.isEmpty(StringUtils.trimWhitespace(pincode));
        log.debug("filter pincode is provided: {} as ", emptyPincode, pincode);
        boolean emptyCountryId = StringUtils.isEmpty(StringUtils.trimWhitespace(countryId));
        log.debug("filter countryId is provided: {} as ", emptyCountryId, countryId);
        boolean emptyCityId = StringUtils.isEmpty(StringUtils.trimWhitespace(cityId));
        log.debug("filter cityId is provided: {} as ", emptyCityId, cityId);
        boolean emptyStateId =  StringUtils.isEmpty(StringUtils.trimWhitespace(stateId));
        log.debug("filter stateId is provided: {} as ", emptyStateId, stateId);
        if(!emptyName || !emptyAddressLine1 || !emptyAddressLine2 || !emptyPincode || !emptyCountryId || !emptyCityId || !emptyStateId) {
            Optional<String> optName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optAddressLine1 = emptyAddressLine1 ? Optional.empty() : Optional.of(addressLine1);
            Optional<String> optAddressLine2 = emptyAddressLine2 ? Optional.empty() : Optional.of(addressLine2);
            Optional<String> optPincode = emptyPincode ? Optional.empty() : Optional.of(pincode);
            Optional<String> optCountryId = emptyCountryId ? Optional.empty() : Optional.of(countryId);
            Optional<String> optCityId = emptyCityId ? Optional.empty() : Optional.of(cityId);
            Optional<String> optStateId = emptyStateId ? Optional.empty() : Optional.of(stateId);
            List<AddressVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(
                    optName, optAddressLine1, optAddressLine2, optPincode, optCityId, optStateId, optCountryId);
            log.debug("Responding with all available addresses with given filters");
            return matchedByFilter;
        }
        log.debug("address filters are empty");
        throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get Address details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Address that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = AddressVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Address id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Address found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public AddressVo getAddressDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws AddressException {
        AddressVo addressDetails = null;
        log.debug("Requesting all details of address by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            addressDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing address details by id");
            return addressDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                addressDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing address details by id wth fields cascaded to given level");
                return addressDetails;
            } catch (NumberFormatException e) {
                log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_CASCADE_LEVEL_EMPTY.getValue());
                throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_ID_EMPTY.getValue());
        throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
