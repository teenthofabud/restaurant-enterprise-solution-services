package com.teenthofabud.restaurant.solution.inventory.product.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.fge.jsonpatch.JsonPatch;
import com.github.fge.jsonpatch.JsonPatchException;
import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.inventory.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.inventory.error.InventoryErrorCode;
import com.teenthofabud.restaurant.solution.inventory.product.converter.ProductDto2EntityConverter;
import com.teenthofabud.restaurant.solution.inventory.product.converter.ProductForm2EntityConverter;
import com.teenthofabud.restaurant.solution.inventory.product.data.*;
import com.teenthofabud.restaurant.solution.inventory.product.mapper.ProductEntitySelfMapper;
import com.teenthofabud.restaurant.solution.inventory.product.mapper.ProductForm2EntityMapper;
import com.teenthofabud.restaurant.solution.inventory.product.repository.ProductRepository;
import com.teenthofabud.restaurant.solution.inventory.product.service.ProductService;
import com.teenthofabud.restaurant.solution.inventory.product.validator.ProductDtoValidator;
import com.teenthofabud.restaurant.solution.inventory.product.validator.ProductFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.inventory.product.validator.ProductFormValidator;
import com.teenthofabud.restaurant.solution.inventory.utils.InventoryServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;

@Component
@Slf4j
public class ProductServiceImpl implements ProductService {

    private static final Comparator<ProductVo> CMP_BY_CATEGORY_ID_AND_NAME = (s1, s2) -> {
        return Integer.compare(s1.getCategoryId().compareTo(s2.getCategoryId()), s1.getName().compareTo(s2.getName()));
    };

    private ProductForm2EntityConverter form2EntityConverter;
    private ProductDto2EntityConverter dto2EntityConverter;
    private ProductForm2EntityMapper form2EntityMapper;
    private ProductEntitySelfMapper entitySelfMapper;
    private ProductFormValidator formValidator;
    private ProductFormRelaxedValidator relaxedFormValidator;
    private ProductDtoValidator dtoValidator;
    private ProductRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private CategoryService categoryService;
    private InventoryServiceHelper inventoryServiceHelper;

    @Autowired
    public void setInventoryServiceHelper(InventoryServiceHelper inventoryServiceHelper) {
        this.inventoryServiceHelper = inventoryServiceHelper;
    }

    @Autowired
    public void setCategoryService(CategoryService categoryService) {
        this.categoryService = categoryService;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setDto2EntityConverter(ProductDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(ProductForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(ProductEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(ProductFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchProductValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(ProductDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(ProductForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(ProductRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(ProductFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseProductId(String id) throws ProductException {
        Long productId = null;
        try {
            productId = Long.parseLong(id);
            log.debug("Parsed id {} to product id {} in numeric format", id, productId);
            if(productId <= 0) {
                throw new NumberFormatException("product id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse product id", e);
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_ID_INVALID.getValue(), id);
            throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return productId;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public Set<ProductVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all ProductEntity by their natural ordering");
        List<ProductEntity> productEntityList = repository.findAll();
        List<ProductVo> productVoList = inventoryServiceHelper.productEntity2DetailedVo(productEntityList);
        Set<ProductVo> naturallyOrderedSet = new TreeSet<ProductVo>(CMP_BY_CATEGORY_ID_AND_NAME);
        naturallyOrderedSet.addAll(productVoList);
        log.info("{} ProductVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public ProductVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws ProductException {
        log.info("Requesting ProductEntity by id: {}", id);
        Long productId = parseProductId(id);
        Optional<ProductEntity> optEntity = repository.findById(productId);
        if(!optEntity.isPresent()) {
            log.debug("No ProductEntity found by id: {}", id);
            throw new ProductException(InventoryErrorCode.INVENTORY_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found ProductVo by id: {}", id);
        ProductEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        ProductVo vo = inventoryServiceHelper.productEntity2DetailedVo(entity);
        log.debug("ProductVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<ProductVo> retrieveAllMatchingDetailsByCategoryId(String categoryId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws ProductException {
        log.info("Requesting ProductEntity that match with categoryId: {}", categoryId);
        Errors err = new DirectFieldBindingResult(categoryId, "ProductForm");
        try {
            CategoryVo categoryVo = categoryService.retrieveDetailsById(categoryId, Optional.of(TOABCascadeLevel.ONE));
            if(!categoryVo.getActive()) {
                throw new ProductException(InventoryErrorCode.INVENTORY_INACTIVE, new Object [] { categoryId });
            }
        } catch (CategoryException e) {
            log.error("categoryId is invalid", e);
            throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object [] { "categoryId: " + categoryId });
        }
        List<ProductEntity> productEntityList = repository.findByCategoryId(Long.parseLong(categoryId));
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        List<ProductVo> matchedProductList = inventoryServiceHelper.productEntity2DetailedVo(productEntityList);
        log.info("Found {} ProductVo matching with categoryId: {}", matchedProductList.size(),categoryId);
        TOABRequestContextHolder.clearCascadeLevelContext();
        if(matchedProductList.isEmpty()) {
            log.debug("No ProductVo found matching with categoryId: {}", categoryId);
            throw new ProductException(InventoryErrorCode.INVENTORY_NOT_FOUND, new Object[] { "categoryId", categoryId });
        }
        return matchedProductList;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<ProductVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName, Optional<String> optionalDescription) throws ProductException {
        if(!optionalName.isPresent() && !optionalDescription.isPresent()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String description = optionalDescription.isPresent() ? optionalDescription.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(description))) {
            log.debug("All search parameters are empty");
        }
        List<ProductVo> matchedProductList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        ProductEntity entity = new ProductEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            log.debug("name {} is valid", name);
            providedFilters.put("name", name);
            entity.setName(name);
            matcherCriteria = matcherCriteria.withMatcher("name", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(description))) {
            log.debug("description {} is valid", description);
            providedFilters.put("description", description);
            entity.setDescription(description);
            matcherCriteria = matcherCriteria.withMatcher("description", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<ProductEntity> productEntityExample = Example.of(entity, matcherCriteria);
        List<ProductEntity> productEntityList = repository.findAll(productEntityExample);
        matchedProductList = inventoryServiceHelper.productEntity2DetailedVo(productEntityList);
        log.info("Found {} ProductVo matching with provided parameters : {}", matchedProductList.size(), providedFilters);
        return matchedProductList;
    }

    @Transactional
    @Override
    public String createProduct(ProductForm form) throws ProductException {
        log.info("Creating new ProductEntity");

        if(form == null) {
            log.debug("ProductForm provided is null");
            throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of ProductForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("ProductForm has {} errors", err.getErrorCount());
            InventoryErrorCode ec = InventoryErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("ProductForm error detail: {}", ec);
            throw new ProductException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of ProductForm are valid");

        ProductEntity expectedEntity = form2EntityConverter.convert(form);

        log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(), form.getName(), form.getCategoryId());
        if(repository.existsByNameAndCategoryId(expectedEntity.getName(), expectedEntity.getCategory().getId())) {
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_EXISTS_BY_NAME_AND_CATEGORY_ID.getValue(), expectedEntity.getName(),
                    expectedEntity.getCategory().getId());
            throw new ProductException(InventoryErrorCode.INVENTORY_EXISTS,
                    new Object[]{ "name: " + form.getName(), ", categoryId: " + form.getCategoryId() });
        }
        log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(), expectedEntity.getName(),
                expectedEntity.getCategory().getId());

        log.debug("Saving {}", expectedEntity);
        ProductEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new ProductException(InventoryErrorCode.INVENTORY_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist ProductForm details" });
        }
        log.info("Created new ProductForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Transactional
    @Override
    public void updateProduct(String id, ProductForm form) throws ProductException {
        log.info("Updating ProductForm by id: {}", id);

        log.debug(ProductMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_PRODUCT_ENTITY_ID.getValue(), id);
        Long productId = parseProductId(id);
        Optional<ProductEntity> optActualEntity = repository.findById(productId);
        if(!optActualEntity.isPresent()) {
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_NO_PRODUCT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ProductException(InventoryErrorCode.INVENTORY_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ProductMessageTemplate.MSG_TEMPLATE_FOUND_PRODUCT_ENTITY_ID.getValue(), id);

        ProductEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("ProductEntity is inactive with id: {}", id);
            throw new ProductException(InventoryErrorCode.INVENTORY_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("ProductEntity is active with id: {}", id);

        if(form == null) {
            log.debug("ProductForm is null");
            throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of ProductForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("ProductForm has {} errors", err.getErrorCount());
            InventoryErrorCode ec = InventoryErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("ProductForm error detail: {}", ec);
            throw new ProductException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of ProductForm are empty");
            throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of ProductForm are valid");

        Optional<ProductEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(!optExpectedEntity.isPresent()) {
            log.debug("No new value for attributes of ProductForm");
            throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from ProductForm to ProductEntity");

        ProductEntity expectedEntity = optExpectedEntity.get();

        checkUniquenessOfProduct(form, actualEntity);

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from ProductEntity to ProductForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new ProductException(InventoryErrorCode.INVENTORY_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency product details" });
        }
        log.info("Updated existing ProductEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void deleteProduct(String id) throws ProductException {
        log.info("Soft deleting ProductEntity by id: {}", id);

        log.debug(ProductMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_PRODUCT_ENTITY_ID.getValue(), id);
        Long productId = parseProductId(id);
        Optional<ProductEntity> optEntity = repository.findById(productId);
        if(!optEntity.isPresent()) {
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_NO_PRODUCT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ProductException(InventoryErrorCode.INVENTORY_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ProductMessageTemplate.MSG_TEMPLATE_FOUND_PRODUCT_ENTITY_ID.getValue(), id);

        ProductEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("ProductEntity is inactive with id: {}", id);
            throw new ProductException(InventoryErrorCode.INVENTORY_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("ProductEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        ProductEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new ProductException(InventoryErrorCode.INVENTORY_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current product details with id:" + id });
        }

        log.info("Soft deleted existing ProductEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void applyPatchOnProduct(String id, List<PatchOperationForm> patches) throws ProductException {
        log.info("Patching ProductEntity by id: {}", id);

        log.debug(ProductMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_PRODUCT_ENTITY_ID.getValue(), id);
        Long productId = parseProductId(id);
        Optional<ProductEntity> optActualEntity = repository.findById(productId);
        if(!optActualEntity.isPresent()) {
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_NO_PRODUCT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ProductException(InventoryErrorCode.INVENTORY_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ProductMessageTemplate.MSG_TEMPLATE_FOUND_PRODUCT_ENTITY_ID.getValue(), id);

        ProductEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Product patch list not provided");
            throw new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Product patch list has {} products", patches.size());


        log.debug("Validating patch list products for Product");
        try {
            toabBaseService.validatePatches(patches, InventoryErrorCode.INVENTORY_EXISTS.getDomain() + ":LOV");
            log.debug("All Product patch list products are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Product patch product are invalid");
            throw new ProductException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list products for Product");


        log.debug("Patching list products to ProductDto");
        ProductDto patchedProductForm = new ProductDto();
        try {
            log.debug("Preparing patch list products for Product");
            JsonNode productDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch productPatch = JsonPatch.fromJson(productDtoTree);
            log.debug("Prepared patch list products for Product");
            JsonNode blankProductDtoTree = om.convertValue(new ProductDto(), JsonNode.class);
            JsonNode patchedProductFormTree = productPatch.apply(blankProductDtoTree);
            log.debug("Applying patch list products to ProductDto");
            patchedProductForm = om.treeToValue(patchedProductFormTree, ProductDto.class);
            log.debug("Applied patch list products to ProductDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list products to ProductDto: {}", e);
            ProductException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in ProductDto");
                ex = new ProductException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new ProductException(InventoryErrorCode.INVENTORY_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list products to ProductDto: {}", e);
            throw new ProductException(InventoryErrorCode.INVENTORY_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list products to ProductDto");

        log.debug("Validating patched ProductDto");
        Errors err = new DirectFieldBindingResult(patchedProductForm, patchedProductForm.getClass().getSimpleName());
        dtoValidator.validate(patchedProductForm, err);
        if(err.hasErrors()) {
            log.debug("Patched ProductDto has {} errors", err.getErrorCount());
            InventoryErrorCode ec = InventoryErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched ProductDto error detail: {}", ec);
            throw new ProductException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched ProductDto are valid");

        checkUniquenessOfProduct(patchedProductForm, actualEntity);

        log.debug("Comparatively copying patched attributes from ProductDto to ProductEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedProductForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (ProductException) e;
        }
        log.debug("Comparatively copied patched attributes from ProductDto to ProductEntity");

        log.debug("Saving patched ProductEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched ProductEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete ProductEntity with id:{}", id);
            throw new ProductException(InventoryErrorCode.INVENTORY_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency product details with id:" + id });
        }
        log.info("Patched ProductEntity with id:{}", id);
    }

    private void checkUniquenessOfProduct(ProductDto patchedProductForm, ProductEntity actualEntity) throws ProductException {
        // name = true, categoryId = false
        if(patchedProductForm.getName().isPresent() && !patchedProductForm.getCategoryId().isPresent()) {
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    patchedProductForm.getName().get(), actualEntity.getCategory().getId());
            boolean sameEntitySw = patchedProductForm.getName().get().compareTo(actualEntity.getName()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCategoryId(patchedProductForm.getName().get(), actualEntity.getCategory().getId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_EXISTS_BY_NAME_AND_CATEGORY_ID.getValue(),
                        patchedProductForm.getName().get(), actualEntity.getCategory().getId());
                throw new ProductException(InventoryErrorCode.INVENTORY_EXISTS,
                        new Object[]{ "name: " + patchedProductForm.getName().get(), ", categoryId: " + actualEntity.getCategory().getId() });
            }
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    patchedProductForm.getName().get(), actualEntity.getCategory().getId());
        }

        // name = true, categoryId = true
        if(patchedProductForm.getName().isPresent() && patchedProductForm.getCategoryId().isPresent()) {
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    patchedProductForm.getName().get(), patchedProductForm.getCategoryId().get());
            boolean sameEntitySw = patchedProductForm.getName().get().compareTo(actualEntity.getName().toString()) == 0
                    && patchedProductForm.getCategoryId().get().compareTo(actualEntity.getCategory().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCategoryId(patchedProductForm.getName().get(),
                    Long.parseLong(patchedProductForm.getCategoryId().get()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_EXISTS_BY_NAME_AND_CATEGORY_ID.getValue(),
                        patchedProductForm.getName().get(), patchedProductForm.getCategoryId().get());
                throw new ProductException(InventoryErrorCode.INVENTORY_EXISTS,
                        new Object[]{ "name: " + patchedProductForm.getName().get(), ", categoryId: " + patchedProductForm.getCategoryId().get() });
            }
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    patchedProductForm.getName().get(), patchedProductForm.getCategoryId().get());
        }

        // name = false, categoryId = true
        if(!patchedProductForm.getName().isPresent() && patchedProductForm.getCategoryId().isPresent()) {
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    actualEntity.getName(),  patchedProductForm.getCategoryId().get());
            boolean sameEntitySw = patchedProductForm.getCategoryId().get().compareTo(actualEntity.getCategory().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCategoryId(
                    actualEntity.getName(), Long.parseLong(patchedProductForm.getCategoryId().get()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_EXISTS_BY_NAME_AND_CATEGORY_ID.getValue(),
                        actualEntity.getName(), patchedProductForm.getCategoryId().get());
                throw new ProductException(InventoryErrorCode.INVENTORY_EXISTS, new Object[]{ "name " + actualEntity.getName(),
                        ", categoryId: " + patchedProductForm.getCategoryId().get() });
            }
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    actualEntity.getName(),  patchedProductForm.getCategoryId().get());
        }
    }

    private void checkUniquenessOfProduct(ProductForm productForm, ProductEntity actualEntity) throws ProductException {
        // name = true, categoryId = false
        if(StringUtils.hasText(StringUtils.trimWhitespace(productForm.getName()))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(productForm.getCategoryId()))) {
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    productForm.getName(), actualEntity.getCategory().getId());
            boolean sameEntitySw = productForm.getName().compareTo(actualEntity.getName()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCategoryId(
                    productForm.getName(), actualEntity.getCategory().getId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_EXISTS_BY_NAME_AND_CATEGORY_ID.getValue(),
                        productForm.getName(), actualEntity.getCategory().getId());
                throw new ProductException(InventoryErrorCode.INVENTORY_EXISTS,
                        new Object[]{ "name: " + productForm.getName(), ", categoryId: " + actualEntity.getCategory().getId() });
            }
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    productForm.getName(), actualEntity.getCategory().getId());
        }

        // name = true, categoryId = true
        if(StringUtils.hasText(StringUtils.trimWhitespace(productForm.getName()))
                && StringUtils.hasText(StringUtils.trimWhitespace(productForm.getCategoryId()))) {
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    productForm.getName(), productForm.getCategoryId());
            boolean sameEntitySw = productForm.getName().compareTo(actualEntity.getName()) == 0
                    && productForm.getCategoryId().compareTo(actualEntity.getCategory().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCategoryId(productForm.getName(), Long.parseLong(productForm.getCategoryId()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_EXISTS_BY_NAME_AND_CATEGORY_ID.getValue(),
                        productForm.getName(), productForm.getCategoryId());
                throw new ProductException(InventoryErrorCode.INVENTORY_EXISTS,
                        new Object[]{ "categoryId: " + productForm.getCategoryId(), ", name: " + productForm.getName() });
            }
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    productForm.getName(), productForm.getCategoryId());
        }

        // name = false, categoryId = true
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(productForm.getName()))
                && StringUtils.hasText(StringUtils.trimWhitespace(productForm.getCategoryId()))) {
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    actualEntity.getName(),  productForm.getCategoryId());
            boolean sameEntitySw = productForm.getName().compareTo(actualEntity.getName()) == 0
                    && productForm.getCategoryId().compareTo(actualEntity.getCategory().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCategoryId(actualEntity.getName(), Long.parseLong(productForm.getCategoryId()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_EXISTS_BY_NAME_AND_CATEGORY_ID.getValue(),
                        actualEntity.getName(),  productForm.getCategoryId());
                throw new ProductException(InventoryErrorCode.INVENTORY_EXISTS, new Object[]{ "name: " + actualEntity.getName(),
                        ", categoryId: " + productForm.getCategoryId() });
            }
            log.debug(ProductMessageTemplate.MSG_TEMPLATE_PRODUCT_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    actualEntity.getName(),  productForm.getCategoryId());
        }
    }

}