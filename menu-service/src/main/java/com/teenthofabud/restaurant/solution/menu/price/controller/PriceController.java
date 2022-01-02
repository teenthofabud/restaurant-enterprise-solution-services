package com.teenthofabud.restaurant.solution.menu.price.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceException;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceForm;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceMessageTemplate;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceVo;
import com.teenthofabud.restaurant.solution.menu.price.service.PriceService;
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
@RequestMapping("price")
@Slf4j
@Tag(name = "Price API", description = "Manage Prices and their details")
public class PriceController {

    private static final String MEDIA_MENU_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(PriceService service) {
        this.service = service;
    }

    private PriceService service;

    @Operation(summary = "Create new Price details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Price",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Price attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Price already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Price attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Price",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewPrice(@RequestBody(required = false) PriceForm form) throws PriceException {
        log.debug("Requesting to create new price");
        if(form != null) {
            String id = service.createPrice(form);
            log.debug("Responding with identifier of newly created new price");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("PriceForm is null");
        throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Price details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Price",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Price attribute's value is invalid/Price is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Price found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Price already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Price details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingPrice(@PathVariable String id, @RequestBody(required = false) PriceForm form) throws PriceException {
        log.debug("Requesting to update all attributes of existing price");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updatePrice(id, form);
                log.debug("Responding with successful updation of attributes for existing price");
                return;
            }
            log.debug("PriceForm is null");
            throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_ID_EMPTY.getValue());
        throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Price by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Price",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Price id is invalid/Price is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Price found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Price attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Price",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingPrice(@PathVariable String id) throws PriceException {
        log.debug("Requesting to soft delete price");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deletePrice(id);
            log.debug("Responding with successful deletion of existing price");
            return;
        }
        log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_ID_EMPTY.getValue());
        throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Price attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Price with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Price attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Price found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Price attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Price with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_MENU_APPLICATION_JSON_PATCH)
    public void patchExistingPrice(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws PriceException {
        log.debug("Requesting to patch of price attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnPrice(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing price");
                return;
            }
            log.debug("price patch document is null");
            throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_ID_EMPTY.getValue());
        throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Price details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Prices and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = PriceVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<PriceVo> getAllPriceNaturallyOrdered() {
        log.debug("Requesting all available prices by their natural orders");
        Set<PriceVo> naturallyOrderedPrices = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available prices by their natural orders");
        return naturallyOrderedPrices;
    }

    @Operation(summary = "Get all Price details by item id", hidden = true)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Pricees and their details that match the given item id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = PriceVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Price id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Pricees available with the given item id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("itemid/{itemId}")
    public List<PriceVo> getAllPricesByItemId(@PathVariable String itemId, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws PriceException {
        List<PriceVo> matchedByItemIds = new ArrayList<>();
        log.debug("Requesting all available addresses with given itemId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(itemId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            matchedByItemIds = service.retrieveAllMatchingDetailsByItemId(itemId, Optional.empty());
            log.debug("Responding with all available addresses with given itemId");
            return matchedByItemIds;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(itemId)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                matchedByItemIds = service.retrieveAllMatchingDetailsByItemId(itemId, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing address details with given itemId having fields cascaded to given level");
                return matchedByItemIds;
            } catch (NumberFormatException e) {
                log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_CASCADE_LEVEL_EMPTY.getValue());
                throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug("address itemId is empty");
        throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "itemId", itemId });
    }

    @Deprecated
    @Operation(summary = "Get all Price details by name, description", deprecated = true, hidden = true)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Prices and their details that match the provided name, description",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = PriceVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Price search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Prices available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("deprecated.filter")
    public List<PriceVo> getAllPricesByFilters(@RequestParam(required = false) String itemId,
                                                   @RequestParam(required = false) String currencyId) throws PriceException {
        log.debug("Requesting all available prices with given filters");
        boolean emptyItemId = !StringUtils.hasText(StringUtils.trimWhitespace(itemId));
        boolean emptyCurrencyId = !StringUtils.hasText(StringUtils.trimWhitespace(currencyId));
        if(!emptyItemId || !emptyCurrencyId) {
            Optional<String> optItemId = emptyItemId ? Optional.empty() : Optional.of(itemId);
            Optional<String> optCurrencyId = emptyCurrencyId ? Optional.empty() : Optional.of(currencyId);
            List<PriceVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optItemId, optCurrencyId);
            log.debug("Responding with all available prices with given filters");
            return matchedByFilter;
        }
        log.debug("price filters are empty");
        throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get all Price details by currencyId")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Prices and their details that match the provided currencyId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = PriceVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Price search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Prices available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<PriceVo> getAllPricesByFilters(@RequestParam(required = false) String currencyId) throws PriceException {
        log.debug("Requesting all available prices with given filters");
        boolean emptyCurrencyId = !StringUtils.hasText(StringUtils.trimWhitespace(currencyId));
        if(!emptyCurrencyId) {
            Optional<String> optCurrencyId = emptyCurrencyId ? Optional.empty() : Optional.of(currencyId);
            List<PriceVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optCurrencyId);
            log.debug("Responding with all available prices with given filters");
            return matchedByFilter;
        }
        log.debug("price filters are empty");
        throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get Price details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Price that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = PriceVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Price id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Price found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public PriceVo getPriceDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws PriceException {
        PriceVo priceDetails = null;
        log.debug("Requesting all details of price by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            priceDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing price details by id");
            return priceDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                priceDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing price details by id wth fields cascaded to given level");
                return priceDetails;
            } catch (NumberFormatException e) {
                log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_CASCADE_LEVEL_EMPTY.getValue());
                throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_ID_EMPTY.getValue());
        throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
