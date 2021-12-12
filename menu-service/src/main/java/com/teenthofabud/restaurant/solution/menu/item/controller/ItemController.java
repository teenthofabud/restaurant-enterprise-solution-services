package com.teenthofabud.restaurant.solution.menu.item.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemException;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemForm;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemMessageTemplate;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemVo;
import com.teenthofabud.restaurant.solution.menu.item.service.ItemService;
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
@RequestMapping("item")
@Slf4j
@Tag(name = "Item API", description = "Manage Items and their details")
public class ItemController {

    private static final String MEDIA_MENU_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(ItemService service) {
        this.service = service;
    }

    private ItemService service;

    @Operation(summary = "Create new Item details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Item",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Item attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Item already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Item attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Item",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewItem(@RequestBody(required = false) ItemForm form) throws ItemException {
        log.debug("Requesting to create new item");
        if(form != null) {
            String id = service.createItem(form);
            log.debug("Responding with identifier of newly created new item");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("ItemForm is null");
        throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Item details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Item",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Item attribute's value is invalid/Item is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Item found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Item already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Item details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingItem(@PathVariable String id, @RequestBody(required = false) ItemForm form) throws ItemException {
        log.debug("Requesting to update all attributes of existing item");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateItem(id, form);
                log.debug("Responding with successful updation of attributes for existing item");
                return;
            }
            log.debug("ItemForm is null");
            throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_ID_EMPTY.getValue());
        throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Item by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Item",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Item id is invalid/Item is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Item found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Item attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Item",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingItem(@PathVariable String id) throws ItemException {
        log.debug("Requesting to soft delete item");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteItem(id);
            log.debug("Responding with successful deletion of existing item");
            return;
        }
        log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_ID_EMPTY.getValue());
        throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Item attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Item with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Item attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Item found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Item attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Item with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_MENU_APPLICATION_JSON_PATCH)
    public void patchExistingItem(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws ItemException {
        log.debug("Requesting to patch of item attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnItem(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing item");
                return;
            }
            log.debug("item patch document is null");
            throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_ID_EMPTY.getValue());
        throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Item details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Items and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = ItemVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<ItemVo> getAllItemNaturallyOrdered() {
        log.debug("Requesting all available items by their natural orders");
        Set<ItemVo> naturallyOrderedItems = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available items by their natural orders");
        return naturallyOrderedItems;
    }

    @Operation(summary = "Get all Item details by category id", hidden = true)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Itemes and their details that match the given category id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = ItemVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Item id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Itemes available with the given category id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("categoryid/{categoryId}")
    public List<ItemVo> getAllItemsByCategoryId(@PathVariable String categoryId, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws ItemException {
        List<ItemVo> matchedByCategoryIds = new ArrayList<>();
        log.debug("Requesting all available addresses with given categoryId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(categoryId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            matchedByCategoryIds = service.retrieveAllMatchingDetailsByCategoryId(categoryId, Optional.empty());
            log.debug("Responding with all available addresses with given categoryId");
            return matchedByCategoryIds;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(categoryId)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                matchedByCategoryIds = service.retrieveAllMatchingDetailsByCategoryId(categoryId, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing address details with given categoryId having fields cascaded to given level");
                return matchedByCategoryIds;
            } catch (NumberFormatException e) {
                log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_CASCADE_LEVEL_EMPTY.getValue());
                throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug("address categoryId is empty");
        throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "categoryId", categoryId });
    }

    @Operation(summary = "Get all Item details by name, description")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Items and their details that match the provided name, description",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = ItemVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Item search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Items available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<ItemVo> getAllItemsByFilters(@RequestParam(required = false) String name,
                                                  @RequestParam(required = false) String description,
                                                  @RequestParam(required = false) String isVegeterian) throws ItemException {
        log.debug("Requesting all available items with given filters");
        boolean emptyName = !StringUtils.hasText(StringUtils.trimWhitespace(name));
        boolean emptyDescription = !StringUtils.hasText(StringUtils.trimWhitespace(description));
        boolean emptyIsVegeterian = !StringUtils.hasText(StringUtils.trimWhitespace(isVegeterian));
        if(!emptyName || !emptyDescription || !emptyIsVegeterian) {
            Optional<String> optName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optDescription = emptyDescription ? Optional.empty() : Optional.of(description);
            Optional<String> optIsVegeterian = emptyIsVegeterian ? Optional.empty() : Optional.of(isVegeterian);
            List<ItemVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optName, optDescription, optIsVegeterian);
            log.debug("Responding with all available items with given filters");
            return matchedByFilter;
        }
        log.debug("item filters are empty");
        throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get Item details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Item that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ItemVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Item id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Item found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public ItemVo getItemDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws ItemException {
        ItemVo itemDetails = null;
        log.debug("Requesting all details of item by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            itemDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing item details by id");
            return itemDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                itemDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing item details by id wth fields cascaded to given level");
                return itemDetails;
            } catch (NumberFormatException e) {
                log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_CASCADE_LEVEL_EMPTY.getValue());
                throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_ID_EMPTY.getValue());
        throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
