package com.teenthofabud.restaurant.solution.cookbook.ingredient.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientException;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientForm;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientMessageTemplate;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientVo;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.service.IngredientService;
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

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@RestController
@RequestMapping("ingredient")
@Slf4j
@Tag(name = "Ingredient API", description = "Manage Ingredients and their details")
public class IngredientController {

    private static final String MEDIA_COOK_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(IngredientService service) {
        this.service = service;
    }

    private IngredientService service;

    @Operation(summary = "Create new Ingredient details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Ingredient",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Ingredient attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Ingredient already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Ingredient attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Ingredient",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewIngredient(@RequestBody(required = false) IngredientForm form) throws IngredientException {
        log.debug("Requesting to create new ingredient");
        if(form != null) {
            String id = service.createIngredient(form);
            log.debug("Responding with identifier of newly created new ingredient");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("IngredientForm is null");
        throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Ingredient details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Ingredient",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Ingredient attribute's value is invalid/Ingredient is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Ingredient found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Ingredient already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Ingredient details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingIngredient(@PathVariable String id, @RequestBody(required = false) IngredientForm form) throws IngredientException {
        log.debug("Requesting to update all attributes of existing ingredient");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateIngredient(id, form);
                log.debug("Responding with successful updation of attributes for existing ingredient");
                return;
            }
            log.debug("IngredientForm is null");
            throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_ID_EMPTY.getValue());
        throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Ingredient by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Ingredient",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Ingredient id is invalid/Ingredient is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Ingredient found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Ingredient attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Ingredient",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingIngredient(@PathVariable String id) throws IngredientException {
        log.debug("Requesting to soft delete ingredient");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteIngredient(id);
            log.debug("Responding with successful deletion of existing ingredient");
            return;
        }
        log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_ID_EMPTY.getValue());
        throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Ingredient attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Ingredient with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Ingredient attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Ingredient found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Ingredient attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Ingredient with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_COOK_APPLICATION_JSON_PATCH)
    public void patchExistingIngredient(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws IngredientException {
        log.debug("Requesting to patch of ingredient attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnIngredient(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing ingredient");
                return;
            }
            log.debug("ingredient patch document is null");
            throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_ID_EMPTY.getValue());
        throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Ingredient details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Ingredients and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = IngredientVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<IngredientVo> getAllIngredientNaturallyOrdered() {
        log.debug("Requesting all available ingredientes by their natural orders");
        Set<IngredientVo> naturallyOrderedIngredients = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available ingredientes by their natural orders");
        return naturallyOrderedIngredients;
    }

    @Operation(summary = "Get all Ingredient details by recipe id", hidden = true)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Ingredientes and their details that match the given recipe id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = IngredientVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Ingredient id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Ingredientes available with the given recipe id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("recipeid/{recipeId}")
    public List<IngredientVo> getAllIngredientsByRecipeId(@PathVariable String recipeId, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws IngredientException {
        List<IngredientVo> matchedByRecipeIds = new ArrayList<>();
        log.debug("Requesting all available ingredientes with given recipeId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(recipeId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            matchedByRecipeIds = service.retrieveAllMatchingDetailsByRecipeId(recipeId, Optional.empty());
            log.debug("Responding with all available ingredientes with given recipeId");
            return matchedByRecipeIds;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(recipeId)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                matchedByRecipeIds = service.retrieveAllMatchingDetailsByRecipeId(recipeId, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing ingredient details with given recipeId having fields cascaded to given level");
                return matchedByRecipeIds;
            } catch (NumberFormatException e) {
                log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_CASCADE_LEVEL_EMPTY.getValue());
                throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug("ingredient recipeId is empty");
        throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "recipeId", recipeId });
    }

    @Operation(summary = "Get all Ingredient details by name, description, product id, quantity amount, quantity unit id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Ingredientes and their details that match the provided name, description, product id, quantity amount, quantity unit id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = IngredientVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Ingredient search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Ingredientes available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<IngredientVo> getAllIngredientesByFilters(@RequestParam(required = false) String name,
                                                          @RequestParam(required = false) String description,
                                                          @RequestParam(required = false) String productId,
                                                          @RequestParam(required = false) String quantityAmount,
                                                          @RequestParam(required = false) String quantityUnitId) throws IngredientException {
        log.debug("Requesting all available ingredientes with given filters");
        boolean emptyName = StringUtils.isEmpty(StringUtils.trimWhitespace(name));
        log.debug("filter name is provided: {} as ", emptyName, name);
        boolean emptyDescription = StringUtils.isEmpty(StringUtils.trimWhitespace(description));
        log.debug("filter description name is provided: {} as ", emptyDescription, description);
        boolean emptyProductId = StringUtils.isEmpty(StringUtils.trimWhitespace(productId));
        log.debug("filter productId is provided: {} as ", emptyProductId, productId);
        boolean emptyQuantityAmount = StringUtils.isEmpty(StringUtils.trimWhitespace(quantityAmount));
        log.debug("filter quantityAmount is provided: {} as ", emptyQuantityAmount, quantityAmount);
        boolean emptyQuantityUnitId =  StringUtils.isEmpty(StringUtils.trimWhitespace(quantityUnitId));
        log.debug("filter quantityUnitId is provided: {} as ", emptyQuantityUnitId, quantityUnitId);
        if(!emptyName || !emptyDescription || !emptyQuantityAmount || !emptyProductId || !emptyProductId || !emptyQuantityUnitId) {
            Optional<String> optName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optDescription = emptyDescription ? Optional.empty() : Optional.of(description);
            Optional<String> optProductId = emptyProductId ? Optional.empty() : Optional.of(productId);
            Optional<String> optQuantityAmount = emptyQuantityAmount ? Optional.empty() : Optional.of(quantityAmount);
            Optional<String> optQuantityUnitId = emptyQuantityUnitId ? Optional.empty() : Optional.of(quantityUnitId);
            List<IngredientVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(
                    optName, optDescription, optProductId, optQuantityAmount, optQuantityUnitId);
            log.debug("Responding with all available ingredientes with given filters");
            return matchedByFilter;
        }
        log.debug("ingredient filters are empty");
        throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get Ingredient details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Ingredient that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = IngredientVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Ingredient id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Ingredient found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public IngredientVo getIngredientDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws IngredientException {
        IngredientVo ingredientDetails = null;
        log.debug("Requesting all details of ingredient by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            ingredientDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing ingredient details by id");
            return ingredientDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                ingredientDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing ingredient details by id wth fields cascaded to given level");
                return ingredientDetails;
            } catch (NumberFormatException e) {
                log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_CASCADE_LEVEL_EMPTY.getValue());
                throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_ID_EMPTY.getValue());
        throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
