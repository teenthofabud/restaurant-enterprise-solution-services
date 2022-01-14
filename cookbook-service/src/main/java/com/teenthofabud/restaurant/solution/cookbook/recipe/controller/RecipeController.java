package com.teenthofabud.restaurant.solution.cookbook.recipe.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeException;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeForm;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeMessageTemplate;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.service.RecipeService;
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
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@RestController
@RequestMapping("recipe")
@Slf4j
@Tag(name = "Recipe API", description = "Manage Recipes and their details")
public class RecipeController {

    private static final String MEDIA_COOK_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(RecipeService service) {
        this.service = service;
    }

    private RecipeService service;

    @Operation(summary = "Create new Recipe details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Recipe",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Recipe attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Recipe already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Recipe attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Recipe",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewRecipe(@RequestBody(required = false) RecipeForm form) throws RecipeException {
        log.debug("Requesting to create new recipe");
        if(form != null) {
            String id = service.createRecipe(form);
            log.debug("Responding with identifier of newly created new recipe");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("RecipeForm is null");
        throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Recipe details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Recipe",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Recipe attribute's value is invalid/Recipe is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Recipe found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Recipe already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Recipe details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingRecipe(@PathVariable String id, @RequestBody(required = false) RecipeForm form) throws RecipeException {
        log.debug("Requesting to update all attributes of existing recipe");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateRecipe(id, form);
                log.debug("Responding with successful updation of attributes for existing recipe");
                return;
            }
            log.debug("RecipeForm is null");
            throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_ID_EMPTY.getValue());
        throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Recipe by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Recipe",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Recipe id is invalid/Recipe is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Recipe found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Recipe attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Recipe",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingRecipe(@PathVariable String id) throws RecipeException {
        log.debug("Requesting to soft delete recipe");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteRecipe(id);
            log.debug("Responding with successful deletion of existing recipe");
            return;
        }
        log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_ID_EMPTY.getValue());
        throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Recipe attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Recipe with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Recipe attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Recipe found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Recipe attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Recipe with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_COOK_APPLICATION_JSON_PATCH)
    public void patchExistingRecipe(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws RecipeException {
        log.debug("Requesting to patch of recipe attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnRecipe(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing recipe");
                return;
            }
            log.debug("recipe patch document is null");
            throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_ID_EMPTY.getValue());
        throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Recipe details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Recipes and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = RecipeVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<RecipeVo> getAllRecipeNaturallyOrdered() {
        log.debug("Requesting all available recipees by their natural orders");
        Set<RecipeVo> naturallyOrderedRecipes = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available recipees by their natural orders");
        return naturallyOrderedRecipes;
    }

    @Operation(summary = "Get all Recipe details by cuisine id", hidden = true)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Recipees and their details that match the given cuisine id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = RecipeVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Recipe id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Recipees available with the given cuisine id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("cuisineid/{cuisineId}")
    public List<RecipeVo> getAllRecipesByCuisineId(@PathVariable String cuisineId, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws RecipeException {
        List<RecipeVo> matchedByCuisineIds = new ArrayList<>();
        log.debug("Requesting all available recipees with given cuisineId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(cuisineId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            matchedByCuisineIds = service.retrieveAllMatchingDetailsByCuisineId(cuisineId, Optional.empty());
            log.debug("Responding with all available recipees with given cuisineId");
            return matchedByCuisineIds;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(cuisineId)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                matchedByCuisineIds = service.retrieveAllMatchingDetailsByCuisineId(cuisineId, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing recipe details with given cuisineId having fields cascaded to given level");
                return matchedByCuisineIds;
            } catch (NumberFormatException e) {
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_CASCADE_LEVEL_EMPTY.getValue());
                throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug("recipe cuisineId is empty");
        throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "cuisineId", cuisineId });
    }

    @Operation(summary = "Get all Recipe details by name, description, instructions, cooking method, item id, preparation time duration, " +
            "preparation time unit id, cooking time duration, cooking time duration id, portion size amount, portion size unit id, number of servings")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Recipees and their details that match the provided name, " +
                    "description, instructions, cooking method, item id, preparation time duration, preparation time unit id, cooking time duration, " +
                    "cooking time duration id, portion size amount, portion size unit id, number of servings",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = RecipeVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Recipe search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Recipees available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<RecipeVo> getAllRecipeesByFilters(@RequestParam(required = false) String name,
                                                  @RequestParam(required = false) String description,
                                                  @RequestParam(required = false) String instructions,
                                                  @RequestParam(required = false) String cookingMethod,
                                                  @RequestParam(required = false) String itemId,
                                                  @RequestParam(required = false) String numberOfServings,
                                                  @RequestParam(required = false) String preparationTimeDuration,
                                                  @RequestParam(required = false) String preparationTimeUnitId,
                                                  @RequestParam(required = false) String cookingTimeDuration,
                                                  @RequestParam(required = false) String cookingTimeUnitId,
                                                  @RequestParam(required = false) String portionSizeAmount,
                                                  @RequestParam(required = false) String portionSizeUnitId) throws RecipeException {
        log.debug("Requesting all available recipees with given filters");
        boolean emptyName = StringUtils.isEmpty(StringUtils.trimWhitespace(name));
        log.debug("filter name is provided: {} as ", emptyName, name);
        boolean emptyDescription = StringUtils.isEmpty(StringUtils.trimWhitespace(description));
        log.debug("filter description name is provided: {} as ", emptyDescription, description);
        boolean emptyInstructions = StringUtils.isEmpty(StringUtils.trimWhitespace(instructions));
        log.debug("filter instructions is provided: {} as ", emptyInstructions, instructions);
        boolean emptyCookingMethod = StringUtils.isEmpty(StringUtils.trimWhitespace(cookingMethod));
        log.debug("filter cookingMethod is provided: {} as ", emptyCookingMethod, cookingMethod);
        boolean emptyNumberOfServings = StringUtils.isEmpty(StringUtils.trimWhitespace(numberOfServings));
        log.debug("filter numberOfServings is provided: {} as ", emptyNumberOfServings, numberOfServings);
        boolean emptyItemId = StringUtils.isEmpty(StringUtils.trimWhitespace(itemId));
        log.debug("filter itemId is provided: {} as ", emptyItemId, itemId);
        boolean emptyPreparationTimeDuration = StringUtils.isEmpty(StringUtils.trimWhitespace(preparationTimeDuration));
        log.debug("filter preparationTimeDuration is provided: {} as ", emptyPreparationTimeDuration, preparationTimeDuration);
        boolean emptyPreparationTimeUnitId =  StringUtils.isEmpty(StringUtils.trimWhitespace(preparationTimeUnitId));
        log.debug("filter preparationTimeUnitId is provided: {} as ", emptyPreparationTimeUnitId, preparationTimeUnitId);
        boolean emptyCookingTimeDuration = StringUtils.isEmpty(StringUtils.trimWhitespace(cookingTimeDuration));
        log.debug("filter cookingTimeDuration is provided: {} as ", emptyCookingTimeDuration, cookingTimeDuration);
        boolean emptyCookingTimeUnitId =  StringUtils.isEmpty(StringUtils.trimWhitespace(cookingTimeUnitId));
        log.debug("filter cookingTimeUnitId is provided: {} as ", emptyCookingTimeUnitId, cookingTimeUnitId);
        boolean emptyPortionSizeAmount = StringUtils.isEmpty(StringUtils.trimWhitespace(portionSizeAmount));
        log.debug("filter portionSizeAmount is provided: {} as ", emptyPortionSizeAmount, portionSizeAmount);
        boolean emptyPortionSizeUnitId =  StringUtils.isEmpty(StringUtils.trimWhitespace(portionSizeUnitId));
        log.debug("filter portionSizeUnitId is provided: {} as ", emptyPortionSizeUnitId, portionSizeUnitId);
        if(!emptyName || !emptyDescription || !emptyInstructions || !emptyCookingMethod || !emptyItemId || !emptyPreparationTimeDuration
                || !emptyPreparationTimeUnitId || !emptyCookingTimeDuration || !emptyCookingTimeUnitId || !emptyPortionSizeAmount || !emptyPortionSizeUnitId) {
            Optional<String> optName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optDescription = emptyDescription ? Optional.empty() : Optional.of(description);
            Optional<String> optInstructions = emptyInstructions ? Optional.empty() : Optional.of(instructions);
            Optional<String> optCookingMethod = emptyCookingMethod ? Optional.empty() : Optional.of(cookingMethod);
            Optional<String> optNumberOfServings = emptyNumberOfServings ? Optional.empty() : Optional.of(numberOfServings);
            Optional<String> optItemId = emptyItemId ? Optional.empty() : Optional.of(itemId);
            Optional<String> optPreparationTimeDuration = emptyPreparationTimeDuration ? Optional.empty() : Optional.of(preparationTimeDuration);
            Optional<String> optPreparationTimeUnitId = emptyPreparationTimeUnitId ? Optional.empty() : Optional.of(preparationTimeUnitId);
            Optional<String> optCookingTimeDuration = emptyCookingTimeDuration ? Optional.empty() : Optional.of(cookingTimeDuration);
            Optional<String> optCookingTimeUnitId = emptyCookingTimeUnitId ? Optional.empty() : Optional.of(cookingTimeUnitId);
            Optional<String> optPortionSizeAmount = emptyPortionSizeAmount ? Optional.empty() : Optional.of(portionSizeAmount);
            Optional<String> optPortionSizeUnitId = emptyPortionSizeUnitId ? Optional.empty() : Optional.of(portionSizeUnitId);
            List<RecipeVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(
                    optName, optDescription, optInstructions, optCookingMethod, optItemId, optNumberOfServings, optPreparationTimeDuration,
                    optPreparationTimeUnitId, optCookingTimeDuration, optCookingTimeUnitId, optPortionSizeAmount, optPortionSizeUnitId);
            log.debug("Responding with all available recipees with given filters");
            return matchedByFilter;
        }
        log.debug("recipe filters are empty");
        throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get Recipe details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Recipe that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RecipeVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Recipe id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Recipe found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public RecipeVo getRecipeDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws RecipeException {
        RecipeVo recipeDetails = null;
        log.debug("Requesting all details of recipe by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            recipeDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing recipe details by id");
            return recipeDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                recipeDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing recipe details by id wth fields cascaded to given level");
                return recipeDetails;
            } catch (NumberFormatException e) {
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_CASCADE_LEVEL_EMPTY.getValue());
                throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_ID_EMPTY.getValue());
        throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
