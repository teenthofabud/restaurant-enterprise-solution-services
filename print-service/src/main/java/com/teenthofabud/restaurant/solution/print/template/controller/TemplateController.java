package com.teenthofabud.restaurant.solution.print.template.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateException;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateForm;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateMessageTemplate;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateVo;
import com.teenthofabud.restaurant.solution.print.template.service.TemplateService;
import com.teenthofabud.restaurant.solution.print.error.PrintErrorCode;
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
@RequestMapping("template")
@Slf4j
@Tag(name = "Template API", description = "Manage Templates and their details")
public class TemplateController {

    private static final String MEDIA_PRINT_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(TemplateService service) {
        this.service = service;
    }

    private TemplateService service;

    @Operation(summary = "Create new Template details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Template",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Template attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Template already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Template attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Template",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewTemplate(@RequestBody(required = false) TemplateForm form) throws TemplateException {
        log.debug("Requesting to create new template");
        if(form != null) {
            String id = service.createTemplate(form);
            log.debug("Responding with identifier of newly created new template");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("TemplateForm is null");
        throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Template details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Template",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Template attribute's value is invalid/Template is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Template found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Template already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Template details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingTemplate(@PathVariable String id, @RequestBody(required = false) TemplateForm form) throws TemplateException {
        log.debug("Requesting to update all attributes of existing template");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateTemplate(id, form);
                log.debug("Responding with successful updation of attributes for existing template");
                return;
            }
            log.debug("TemplateForm is null");
            throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_ID_EMPTY.getValue());
        throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Template by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Template",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Template id is invalid/Template is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Template found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Template attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Template",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingTemplate(@PathVariable String id) throws TemplateException {
        log.debug("Requesting to soft delete template");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteTemplate(id);
            log.debug("Responding with successful deletion of existing template");
            return;
        }
        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_ID_EMPTY.getValue());
        throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Template attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Template with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Template attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Template found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Template attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Template with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_PRINT_APPLICATION_JSON_PATCH)
    public void patchExistingTemplate(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws TemplateException {
        log.debug("Requesting to patch of template attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnTemplate(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing template");
                return;
            }
            log.debug("template patch document is null");
            throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_ID_EMPTY.getValue());
        throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Template details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Templates and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = TemplateVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<TemplateVo> getAllTemplateNaturallyOrdered() {
        log.debug("Requesting all available categories by their natural orders");
        Set<TemplateVo> naturallyOrderedTemplates = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available categories by their natural orders");
        return naturallyOrderedTemplates;
    }

    @Operation(summary = "Get all Template details by name, description")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Templates and their details that match the provided name, description",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = TemplateVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Template search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Templates available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<TemplateVo> getAllTemplatesByFilters(@RequestParam(required = false) String name, 
                                                     @RequestParam(required = false) String description,
                                                     @RequestParam(required = false) String templateTypeId) throws TemplateException {
        log.debug("Requesting all available categories with given filters");
        boolean emptyName = !StringUtils.hasText(StringUtils.trimWhitespace(name));
        boolean emptyDescription = !StringUtils.hasText(StringUtils.trimWhitespace(description));
        boolean emptyTemplateTypeId = !StringUtils.hasText(StringUtils.trimWhitespace(templateTypeId));
        if(!emptyName || !emptyDescription || !emptyTemplateTypeId) {
            Optional<String> optName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optDescription = emptyDescription ? Optional.empty() : Optional.of(description);
            Optional<String> optTemplateTypeId = emptyTemplateTypeId ? Optional.empty() : Optional.of(templateTypeId);
            List<TemplateVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optName, optDescription, optTemplateTypeId);
            log.debug("Responding with all available categories with given filters");
            return matchedByFilter;
        }
        log.debug("template filters are empty");
        throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get Template details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Template that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = TemplateVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Template id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Template found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public TemplateVo getTemplateDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws TemplateException {
        TemplateVo templateDetails = null;
        log.debug("Requesting all details of template by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            templateDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing template details by id");
            return templateDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                templateDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing template details by id wth fields cascaded to given level");
                return templateDetails;
            } catch (NumberFormatException e) {
                log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_CASCADE_LEVEL_EMPTY.getValue());
                throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(TemplateMessageTemplate.MSG_TEMPLATE_TEMPLATE_ID_EMPTY.getValue());
        throw new TemplateException(PrintErrorCode.PRINT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
