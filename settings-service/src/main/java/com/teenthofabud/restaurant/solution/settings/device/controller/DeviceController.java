package com.teenthofabud.restaurant.solution.settings.device.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceException;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceForm;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceMessageTemplate;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceVo;
import com.teenthofabud.restaurant.solution.settings.device.service.DeviceService;
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
@RequestMapping("device")
@Slf4j
@Tag(name = "Device API", description = "Manage Devices and their details")
public class DeviceController {

    private static final String MEDIA_SETTINGS_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(DeviceService service) {
        this.service = service;
    }

    private DeviceService service;

    @Operation(summary = "Create new Device details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Device",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Device attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Device already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Device attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Device",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewDevice(@RequestBody(required = false) DeviceForm form) throws DeviceException {
        log.debug("Requesting to create new device");
        if(form != null) {
            String id = service.createDevice(form);
            log.debug("Responding with identifier of newly created new device");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("DeviceForm is null");
        throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Device details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Device",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Device attribute's value is invalid/Device is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Device found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Device already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Device details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingDevice(@PathVariable String id, @RequestBody(required = false) DeviceForm form) throws DeviceException {
        log.debug("Requesting to update all attributes of existing device");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateDevice(id, form);
                log.debug("Responding with successful updation of attributes for existing device");
                return;
            }
            log.debug("DeviceForm is null");
            throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_ID_EMPTY.getValue());
        throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Device by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Device",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Device id is invalid/Device is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Device found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Device attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Device",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingDevice(@PathVariable String id) throws DeviceException {
        log.debug("Requesting to soft delete device");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteDevice(id);
            log.debug("Responding with successful deletion of existing device");
            return;
        }
        log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_ID_EMPTY.getValue());
        throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Device attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Device with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Device attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Device found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Device attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Device with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_SETTINGS_APPLICATION_JSON_PATCH)
    public void patchExistingDevice(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws DeviceException {
        log.debug("Requesting to patch of device attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnDevice(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing device");
                return;
            }
            log.debug("device patch document is null");
            throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_ID_EMPTY.getValue());
        throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Device details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Devices and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = DeviceVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<DeviceVo> getAllDeviceNaturallyOrdered() {
        log.debug("Requesting all available categories by their natural orders");
        Set<DeviceVo> naturallyOrderedDevices = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available categories by their natural orders");
        return naturallyOrderedDevices;
    }

    @Operation(summary = "Get all Device details by name, description")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Devices and their details that match the provided name, description",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = DeviceVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Device search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Devices available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<DeviceVo> getAllDevicesByFilters(@RequestParam(required = false) String name,
                                                   @RequestParam(required = false) String description,
                                                   @RequestParam(required = false) String deviceTypeId) throws DeviceException {
        log.debug("Requesting all available categories with given filters");
        boolean emptyName = !StringUtils.hasText(StringUtils.trimWhitespace(name));
        boolean emptyDescription = !StringUtils.hasText(StringUtils.trimWhitespace(description));
        boolean emptyDeviceTypeId = !StringUtils.hasText(StringUtils.trimWhitespace(deviceTypeId));
        if(!emptyName || !emptyDescription || !emptyDeviceTypeId) {
            Optional<String> optName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optDescription = emptyDescription ? Optional.empty() : Optional.of(description);
            Optional<String> optDeviceTypeId = emptyDeviceTypeId ? Optional.empty() : Optional.of(deviceTypeId);
            List<DeviceVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optName, optDescription, optDeviceTypeId);
            log.debug("Responding with all available categories with given filters");
            return matchedByFilter;
        }
        log.debug("device filters are empty");
        throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get Device details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Device that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = DeviceVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Device id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Device found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public DeviceVo getDeviceDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws DeviceException {
        DeviceVo deviceDetails = null;
        log.debug("Requesting all details of device by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            deviceDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing device details by id");
            return deviceDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                deviceDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing device details by id wth fields cascaded to given level");
                return deviceDetails;
            } catch (NumberFormatException e) {
                log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_CASCADE_LEVEL_EMPTY.getValue());
                throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(DeviceMessageTemplate.MSG_TEMPLATE_DEVICE_ID_EMPTY.getValue());
        throw new DeviceException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
