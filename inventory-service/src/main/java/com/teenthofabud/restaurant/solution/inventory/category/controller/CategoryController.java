package com.teenthofabud.restaurant.solution.inventory.category.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.inventory.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryForm;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryMessageTemplate;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.inventory.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.inventory.error.InventoryErrorCode;
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
@RequestMapping("category")
@Slf4j
@Tag(name = "Category API", description = "Manage Categories and their details")
public class CategoryController {

    private static final String MEDIA_INVENTORY_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(CategoryService service) {
        this.service = service;
    }

    private CategoryService service;

    @Operation(summary = "Create new Category details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Category",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Category attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Category already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Category attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Category",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewCategory(@RequestBody(required = false) CategoryForm form) throws CategoryException {
        log.debug("Requesting to create new category");
        if(form != null) {
            String id = service.createCategory(form);
            log.debug("Responding with identifier of newly created new category");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("CategoryForm is null");
        throw new CategoryException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Category details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Category",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Category attribute's value is invalid/Category is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Category found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Category already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Category details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingCategory(@PathVariable String id, @RequestBody(required = false) CategoryForm form) throws CategoryException {
        log.debug("Requesting to update all attributes of existing category");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateCategory(id, form);
                log.debug("Responding with successful updation of attributes for existing category");
                return;
            }
            log.debug("CategoryForm is null");
            throw new CategoryException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_ID_EMPTY.getValue());
        throw new CategoryException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Category by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Category",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Category id is invalid/Category is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Category found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Category attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Category",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingCategory(@PathVariable String id) throws CategoryException {
        log.debug("Requesting to soft delete category");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteCategory(id);
            log.debug("Responding with successful deletion of existing category");
            return;
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_ID_EMPTY.getValue());
        throw new CategoryException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Category attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Category with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Category attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Category found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Category attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Category with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_INVENTORY_APPLICATION_JSON_PATCH)
    public void patchExistingCategory(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws CategoryException {
        log.debug("Requesting to patch of category attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnCategory(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing category");
                return;
            }
            log.debug("category patch document is null");
            throw new CategoryException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_ID_EMPTY.getValue());
        throw new CategoryException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Category details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Categories and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = CategoryVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<CategoryVo> getAllCategoryNaturallyOrdered() {
        log.debug("Requesting all available categories by their natural orders");
        Set<CategoryVo> naturallyOrderedCategories = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available categories by their natural orders");
        return naturallyOrderedCategories;
    }

    @Operation(summary = "Get all Category details by name, description")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Categories and their details that match the provided name, description",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = CategoryVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Category search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Categories available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<CategoryVo> getAllCategoriesByFilters(@RequestParam(required = false) String name, 
                                                     @RequestParam(required = false) String description) throws CategoryException {
        log.debug("Requesting all available categories with given filters");
        boolean emptyName = !StringUtils.hasText(StringUtils.trimWhitespace(name));
        boolean emptyDescription = !StringUtils.hasText(StringUtils.trimWhitespace(description));
        if(!emptyName || !emptyDescription) {
            Optional<String> optName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optDescription = emptyDescription ? Optional.empty() : Optional.of(description);
            List<CategoryVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optName, optDescription);
            log.debug("Responding with all available categories with given filters");
            return matchedByFilter;
        }
        log.debug("category filters are empty");
        throw new CategoryException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get Category details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Category that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CategoryVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Category id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Category found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public CategoryVo getCategoryDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws CategoryException {
        CategoryVo categoryDetails = null;
        log.debug("Requesting all details of category by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            categoryDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing category details by id");
            return categoryDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                categoryDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing category details by id wth fields cascaded to given level");
                return categoryDetails;
            } catch (NumberFormatException e) {
                log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_CASCADE_LEVEL_EMPTY.getValue());
                throw new CategoryException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_ID_EMPTY.getValue());
        throw new CategoryException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
