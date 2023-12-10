package com.teenthofabud.restaurant.solution.settings.internationalization.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerVo;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountVo;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import com.teenthofabud.restaurant.solution.settings.internationalization.data.LanguageException;
import com.teenthofabud.restaurant.solution.settings.internationalization.data.LanguageForm;
import com.teenthofabud.restaurant.solution.settings.internationalization.data.LanguageTemplate;
import com.teenthofabud.restaurant.solution.settings.internationalization.data.LanguageVo;
import com.teenthofabud.restaurant.solution.settings.internationalization.service.InternationalizationService;
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
@RequestMapping("language")
@Slf4j
@Tag(name = "Internationalization API", description = "Get all the language details")
public class LanguageController {
    private static final String MEDIA_SETTINGS_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    private InternationalizationService languageService;

    @Operation(summary = "Get all language details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Languages and their details ordered by name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = LanguageVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<LanguageVo> getAllLanguageByNaturalOrder(){
        log.debug("Requesting all language by natural order");
        Set<LanguageVo> naturallyOrderedLanguages = languageService.retrieveAllByNaturalOrdering();
        log.debug("Responding all language by natural order");
        return naturallyOrderedLanguages;
    }

    @Operation(summary = "Create new languages by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created language",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = LanguageVo.class))}),
            @ApiResponse(responseCode = "400", description = "Language attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Language already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Language attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Language",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo PostNewLanguage(@RequestBody(required = false)LanguageForm form) throws LanguageException {
        log.debug("Requesting to create new language");
        if(form != null){
            String id = languageService.createLanguage(form);
            log.debug("Responding with newly created new language");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("Language form is null");
        throw new LanguageException(SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED,
                new Object[] {"form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED});
    }

    @Operation(summary = "Get Language details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Language that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = LanguageVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Language id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Language found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public LanguageVo getLanguageById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
    String cascadeUnitLevel) throws LanguageException {
        LanguageVo languageDetails = null;
        log.debug("Requesting all details of language by id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(cascadeUnitLevel)){
            languageDetails = languageService.retrieveLanguageById(id, Optional.empty());
            log.debug("Responding with successful retrieval of language by id");
            return languageDetails;
        }else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUnitLevel))){
            Integer cascadeLevelCode = Integer.parseInt(cascadeUnitLevel);
            if(cascadeLevelCode < 0){
                throw new NumberFormatException();
            }
            log.debug("Request with cascade level code {}", cascadeLevelCode);
            Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUnitLevel);
            languageDetails = languageService.retrieveLanguageById(id, optCascadeLevel);
            log.debug("Responding with successful retrieval of existing deliveryPartner details by id wth fields cascaded to given level");
            return languageDetails;
        }
        log.debug(LanguageTemplate.MSG_TEMPLATE_LANGUAGE_ID_EMPTY.getValue());
        throw new LanguageException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[]{"id", id});
    }
    @Operation(summary = "Get all language details by name, code")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available languages and their details that match the provided name, code",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = DiscountVo.class))) }),
            @ApiResponse(responseCode = "400", description = "language search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No languages available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("/filter")
    public List<LanguageVo> getAllLanguagesByFilters(@RequestParam(required = false) String name,
                                                     @RequestParam(required = false) String code
    ) throws LanguageException {
        log.debug("Retrieving all languages by given filters");

        boolean languageName = !StringUtils.hasText(StringUtils.trimWhitespace(name));
        boolean languageCode = !StringUtils.hasText(StringUtils.trimWhitespace(code));

        if(!languageCode || !languageName){
            List<LanguageVo> matchedByFilter = languageService.retrieveAllMatchingDetailsByCriteria(name,
                    code);
            log.info("Responding with all available languages wth given filters");
            return matchedByFilter;
        }
        log.debug("language filters are empty");
        throw new LanguageException(SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID, new Object[] {"filters"});
    }
}
