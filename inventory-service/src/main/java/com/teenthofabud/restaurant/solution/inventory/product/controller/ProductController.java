package com.teenthofabud.restaurant.solution.inventory.product.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.inventory.error.InventoryErrorCode;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductException;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductForm;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductMessageTemplate;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductVo;
import com.teenthofabud.restaurant.solution.inventory.product.service.ProductService;
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
@RequestMapping("product")
@Slf4j
@Tag(name = "Product API", description = "Manage Products and their details")
public class ProductController {

    private static final String MEDIA_INVENTORY_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(ProductService service) {
        this.service = service;
    }

    private ProductService service;

    @Operation(summary = "Create new Product details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Product",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Product attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Product already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Product attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Product",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewProduct(@RequestBody(required = false) ProductForm form) throws ProductException {
        log.debug("Requesting to create new product");
        if(form != null) {
            String id = service.createProduct(form);
            log.debug("Responding with identifier of newly created new product");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("ProductForm is null");
        throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Product details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Product",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Product attribute's value is invalid/Product is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Product found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Product already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Product details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingProduct(@PathVariable String id, @RequestBody(required = false) ProductForm form) throws ProductException {
        log.debug("Requesting to update all attributes of existing product");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateProduct(id, form);
                log.debug("Responding with successful updation of attributes for existing product");
                return;
            }
            log.debug("ProductForm is null");
            throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_ID_EMPTY.getValue());
        throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Product by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Product",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Product id is invalid/Product is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Product found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Product attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Product",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingProduct(@PathVariable String id) throws ProductException {
        log.debug("Requesting to soft delete product");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteProduct(id);
            log.debug("Responding with successful deletion of existing product");
            return;
        }
        log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_ID_EMPTY.getValue());
        throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Product attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Product with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Product attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Product found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Product attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Product with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_INVENTORY_APPLICATION_JSON_PATCH)
    public void patchExistingProduct(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws ProductException {
        log.debug("Requesting to patch of product attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnProduct(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing product");
                return;
            }
            log.debug("product patch document is null");
            throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_ID_EMPTY.getValue());
        throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Product details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Products and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = ProductVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<ProductVo> getAllProductNaturallyOrdered() {
        log.debug("Requesting all available products by their natural orders");
        Set<ProductVo> naturallyOrderedProducts = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available products by their natural orders");
        return naturallyOrderedProducts;
    }

    @Operation(summary = "Get all Product details by category id", hidden = true)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Productes and their details that match the given category id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = ProductVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Product id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Productes available with the given category id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("categoryid/{categoryId}")
    public List<ProductVo> getAllProductsByCategoryId(@PathVariable String categoryId, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws ProductException {
        List<ProductVo> matchedByCategoryIds = new ArrayList<>();
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
                log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_CASCADE_LEVEL_EMPTY.getValue());
                throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug("address categoryId is empty");
        throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "categoryId", categoryId });
    }

    @Operation(summary = "Get all Product details by name, description")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Products and their details that match the provided name, description",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = ProductVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Product search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Products available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<ProductVo> getAllProductsByFilters(@RequestParam(required = false) String name,
                                                      @RequestParam(required = false) String description) throws ProductException {
        log.debug("Requesting all available products with given filters");
        boolean emptyName = !StringUtils.hasText(StringUtils.trimWhitespace(name));
        boolean emptyDescription = !StringUtils.hasText(StringUtils.trimWhitespace(description));
        if(!emptyName || !emptyDescription) {
            Optional<String> optName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optDescription = emptyDescription ? Optional.empty() : Optional.of(description);
            List<ProductVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(optName, optDescription);
            log.debug("Responding with all available products with given filters");
            return matchedByFilter;
        }
        log.debug("product filters are empty");
        throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get Product details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Product that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProductVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Product id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Product found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public ProductVo getProductDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws ProductException {
        ProductVo productDetails = null;
        log.debug("Requesting all details of product by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            productDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing product details by id");
            return productDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                productDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing product details by id wth fields cascaded to given level");
                return productDetails;
            } catch (NumberFormatException e) {
                log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_CASCADE_LEVEL_EMPTY.getValue());
                throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_ID_EMPTY.getValue());
        throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
