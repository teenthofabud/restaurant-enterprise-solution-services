package com.teenthofabud.restaurant.solution.inventory.quantity.service.impl;

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
import com.teenthofabud.restaurant.solution.inventory.error.InventoryErrorCode;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductEntity;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductException;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductVo;
import com.teenthofabud.restaurant.solution.inventory.product.repository.ProductRepository;
import com.teenthofabud.restaurant.solution.inventory.product.service.ProductService;
import com.teenthofabud.restaurant.solution.inventory.quantity.converter.QuantityDto2EntityConverter;
import com.teenthofabud.restaurant.solution.inventory.quantity.converter.QuantityForm2EntityConverter;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.*;
import com.teenthofabud.restaurant.solution.inventory.quantity.mapper.QuantityEntitySelfMapper;
import com.teenthofabud.restaurant.solution.inventory.quantity.mapper.QuantityForm2EntityMapper;
import com.teenthofabud.restaurant.solution.inventory.quantity.repository.QuantityRepository;
import com.teenthofabud.restaurant.solution.inventory.quantity.service.QuantityService;
import com.teenthofabud.restaurant.solution.inventory.quantity.validator.QuantityDtoValidator;
import com.teenthofabud.restaurant.solution.inventory.quantity.validator.QuantityFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.inventory.quantity.validator.QuantityFormValidator;
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
public class QuantityServiceImpl implements QuantityService {

    private static final Comparator<QuantityVo> CMP_BY_CATEGORY_ID_AND_NAME = (s1, s2) -> {
        return Integer.compare(s1.getProductId().compareTo(s2.getProductId()), s1.getWeightId().compareTo(s2.getWeightId()));
    };

    private QuantityForm2EntityConverter form2EntityConverter;
    private QuantityDto2EntityConverter dto2EntityConverter;
    private QuantityForm2EntityMapper form2EntityMapper;
    private QuantityEntitySelfMapper entitySelfMapper;
    private QuantityFormValidator formValidator;
    private QuantityFormRelaxedValidator relaxedFormValidator;
    private QuantityDtoValidator dtoValidator;
    private QuantityRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private ProductService productService;
    private ProductRepository productRepository;
    private InventoryServiceHelper inventoryServiceHelper;

    @Autowired
    public void setProductRepository(ProductRepository productRepository) {
        this.productRepository = productRepository;
    }

    @Autowired
    public void setInventoryServiceHelper(InventoryServiceHelper inventoryServiceHelper) {
        this.inventoryServiceHelper = inventoryServiceHelper;
    }

    @Autowired
    public void setProductService(ProductService productService) {
        this.productService = productService;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setDto2EntityConverter(QuantityDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(QuantityForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(QuantityEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(QuantityFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchQuantityValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(QuantityDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(QuantityForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(QuantityRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(QuantityFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseQuantityId(String id) throws QuantityException {
        Long quantityId = null;
        try {
            quantityId = Long.parseLong(id);
            log.debug("Parsed id {} to quantity id {} in numeric format", id, quantityId);
            if(quantityId <= 0) {
                throw new NumberFormatException("quantity id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse quantity id", e);
            log.debug(QuantityMessageTemplate.MSG_TEMPLATE_QUANTITY_ID_INVALID.getValue(), id);
            throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return quantityId;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public Set<QuantityVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all QuantityEntity by their natural ordering");
        List<QuantityEntity> quantityEntityList = repository.findAll();
        List<QuantityVo> quantityVoList = inventoryServiceHelper.quantityEntity2DetailedVo(quantityEntityList);
        Set<QuantityVo> naturallyOrderedSet = new TreeSet<QuantityVo>(CMP_BY_CATEGORY_ID_AND_NAME);
        naturallyOrderedSet.addAll(quantityVoList);
        log.info("{} QuantityVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public QuantityVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws QuantityException {
        log.info("Requesting QuantityEntity by id: {}", id);
        Long quantityId = parseQuantityId(id);
        Optional<QuantityEntity> optEntity = repository.findById(quantityId);
        if(optEntity.isEmpty()) {
            log.debug("No QuantityEntity found by id: {}", id);
            throw new QuantityException(InventoryErrorCode.INVENTORY_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found QuantityVo by id: {}", id);
        QuantityEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        QuantityVo vo = inventoryServiceHelper.quantityEntity2DetailedVo(entity);
        log.debug("QuantityVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<QuantityVo> retrieveAllMatchingDetailsByProductId(String productId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws QuantityException {
        log.info("Requesting QuantityEntity that match with productId: {}", productId);
        Errors err = new DirectFieldBindingResult(productId, "QuantityForm");
        try {
            ProductVo productVo = productService.retrieveDetailsById(productId, Optional.of(TOABCascadeLevel.ONE));
            if(!productVo.getActive()) {
                throw new QuantityException(InventoryErrorCode.INVENTORY_INACTIVE, new Object [] { productId });
            }
        } catch (ProductException e) {
            log.error("productId is invalid", e);
            throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object [] { "productId: " + productId });
        }
        List<QuantityEntity> quantityEntityList = repository.findByProductId(Long.parseLong(productId));
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        List<QuantityVo> matchedQuantityList = inventoryServiceHelper.quantityEntity2DetailedVo(quantityEntityList);
        log.info("Found {} QuantityVo matching with productId: {}", matchedQuantityList.size(),productId);
        TOABRequestContextHolder.clearCascadeLevelContext();
        if(matchedQuantityList.isEmpty()) {
            log.debug("No QuantityVo found matching with productId: {}", productId);
            throw new QuantityException(InventoryErrorCode.INVENTORY_NOT_FOUND, new Object[] { "productId", productId });
        }
        return matchedQuantityList;
    }

    @Override
    public List<QuantityVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalProductId, Optional<String> optionalWeightId) throws QuantityException {
        if(optionalProductId.isEmpty() && optionalWeightId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String productId = optionalProductId.isPresent() ? optionalProductId.get() : "";
        String weightId = optionalWeightId.isPresent() ? optionalWeightId.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(productId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(weightId))) {
            log.debug("All search parameters are empty");
        }
        List<QuantityVo> matchedQuantityList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        QuantityEntity entity = new QuantityEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(productId))) {
            try {
                productService.retrieveDetailsById(productId, Optional.empty());
            } catch (ProductException e) {
                log.error("productId parameter is invalid", e);
                throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "productId", productId });
            }
            Optional<ProductEntity> optionalProductEntity = productRepository.findById(Long.parseLong(productId));
            log.debug("productId {} is valid", productId);
            providedFilters.put("productId", productId);
            entity.setProduct(optionalProductEntity.get());
            matcherCriteria = matcherCriteria.withMatcher("productId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(weightId))) {
            if(!inventoryServiceHelper.isWeightCodeValid(weightId)) {
                log.error("weightId parameter is invalid");
                throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[] { "weightId", weightId });
            }
            log.debug("weightId {} is valid", weightId);
            providedFilters.put("weightId", weightId);
            entity.setWeightId(weightId);
            matcherCriteria = matcherCriteria.withMatcher("weightId", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<QuantityEntity> quantityEntityExample = Example.of(entity, matcherCriteria);
        List<QuantityEntity> quantityEntityList = repository.findAll(quantityEntityExample);
        matchedQuantityList = inventoryServiceHelper.quantityEntity2DetailedVo(quantityEntityList);
        log.info("Found {} QuantityVo matching with provided parameters : {}", matchedQuantityList.size(), providedFilters);
        return matchedQuantityList;
    }

    @Transactional
    @Override
    public String createQuantity(QuantityForm form) throws QuantityException {
        log.info("Creating new QuantityEntity");

        if(form == null) {
            log.debug("QuantityForm provided is null");
            throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of QuantityForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("QuantityForm has {} errors", err.getErrorCount());
            InventoryErrorCode ec = InventoryErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("QuantityForm error detail: {}", ec);
            throw new QuantityException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of QuantityForm are valid");

        QuantityEntity expectedEntity = form2EntityConverter.convert(form);

        log.debug("Saving {}", expectedEntity);
        QuantityEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new QuantityException(InventoryErrorCode.INVENTORY_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist QuantityForm details" });
        }
        log.info("Created new QuantityForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Transactional
    @Override
    public void updateQuantity(String id, QuantityForm form) throws QuantityException {
        log.info("Updating QuantityForm by id: {}", id);

        log.debug(QuantityMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_QUANTITY_ENTITY_ID.getValue(), id);
        Long quantityId = parseQuantityId(id);
        Optional<QuantityEntity> optActualEntity = repository.findById(quantityId);
        if(optActualEntity.isEmpty()) {
            log.debug(QuantityMessageTemplate.MSG_TEMPLATE_NO_QUANTITY_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new QuantityException(InventoryErrorCode.INVENTORY_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(QuantityMessageTemplate.MSG_TEMPLATE_FOUND_QUANTITY_ENTITY_ID.getValue(), id);

        QuantityEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("QuantityEntity is inactive with id: {}", id);
            throw new QuantityException(InventoryErrorCode.INVENTORY_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("QuantityEntity is active with id: {}", id);

        if(form == null) {
            log.debug("QuantityForm is null");
            throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of QuantityForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("QuantityForm has {} errors", err.getErrorCount());
            InventoryErrorCode ec = InventoryErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("QuantityForm error detail: {}", ec);
            throw new QuantityException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of QuantityForm are empty");
            throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of QuantityForm are valid");

        Optional<QuantityEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of QuantityForm");
            throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from QuantityForm to QuantityEntity");

        QuantityEntity expectedEntity = optExpectedEntity.get();

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from QuantityEntity to QuantityForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new QuantityException(InventoryErrorCode.INVENTORY_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist weight quantity details" });
        }
        log.info("Updated existing QuantityEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void deleteQuantity(String id) throws QuantityException {
        log.info("Soft deleting QuantityEntity by id: {}", id);

        log.debug(QuantityMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_QUANTITY_ENTITY_ID.getValue(), id);
        Long quantityId = parseQuantityId(id);
        Optional<QuantityEntity> optEntity = repository.findById(quantityId);
        if(optEntity.isEmpty()) {
            log.debug(QuantityMessageTemplate.MSG_TEMPLATE_NO_QUANTITY_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new QuantityException(InventoryErrorCode.INVENTORY_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(QuantityMessageTemplate.MSG_TEMPLATE_FOUND_QUANTITY_ENTITY_ID.getValue(), id);

        QuantityEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("QuantityEntity is inactive with id: {}", id);
            throw new QuantityException(InventoryErrorCode.INVENTORY_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("QuantityEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        QuantityEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new QuantityException(InventoryErrorCode.INVENTORY_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current quantity details with id:" + id });
        }

        log.info("Soft deleted existing QuantityEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void applyPatchOnQuantity(String id, List<PatchOperationForm> patches) throws QuantityException {
        log.info("Patching QuantityEntity by id: {}", id);

        log.debug(QuantityMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_QUANTITY_ENTITY_ID.getValue(), id);
        Long quantityId = parseQuantityId(id);
        Optional<QuantityEntity> optActualEntity = repository.findById(quantityId);
        if(optActualEntity.isEmpty()) {
            log.debug(QuantityMessageTemplate.MSG_TEMPLATE_NO_QUANTITY_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new QuantityException(InventoryErrorCode.INVENTORY_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(QuantityMessageTemplate.MSG_TEMPLATE_FOUND_QUANTITY_ENTITY_ID.getValue(), id);

        QuantityEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Quantity patch list not provided");
            throw new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Quantity patch list has {} quantitys", patches.size());


        log.debug("Validating patch list quantitys for Quantity");
        try {
            toabBaseService.validatePatches(patches, InventoryErrorCode.INVENTORY_EXISTS.getDomain() + ":LOV");
            log.debug("All Quantity patch list quantitys are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Quantity patch quantity are invalid");
            throw new QuantityException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list quantitys for Quantity");


        log.debug("Patching list quantitys to QuantityDto");
        QuantityDto patchedQuantityForm = new QuantityDto();
        try {
            log.debug("Preparing patch list quantitys for Quantity");
            JsonNode quantityDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch quantityPatch = JsonPatch.fromJson(quantityDtoTree);
            log.debug("Prepared patch list quantitys for Quantity");
            JsonNode blankQuantityDtoTree = om.convertValue(new QuantityDto(), JsonNode.class);
            JsonNode patchedQuantityFormTree = quantityPatch.apply(blankQuantityDtoTree);
            log.debug("Applying patch list quantitys to QuantityDto");
            patchedQuantityForm = om.treeToValue(patchedQuantityFormTree, QuantityDto.class);
            log.debug("Applied patch list quantitys to QuantityDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list quantitys to QuantityDto: {}", e);
            QuantityException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in QuantityDto");
                ex = new QuantityException(InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new QuantityException(InventoryErrorCode.INVENTORY_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list quantitys to QuantityDto: {}", e);
            throw new QuantityException(InventoryErrorCode.INVENTORY_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list quantitys to QuantityDto");

        log.debug("Validating patched QuantityDto");
        Errors err = new DirectFieldBindingResult(patchedQuantityForm, patchedQuantityForm.getClass().getSimpleName());
        dtoValidator.validate(patchedQuantityForm, err);
        if(err.hasErrors()) {
            log.debug("Patched QuantityDto has {} errors", err.getErrorCount());
            InventoryErrorCode ec = InventoryErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched QuantityDto error detail: {}", ec);
            throw new QuantityException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched QuantityDto are valid");

        log.debug("Comparatively copying patched attributes from QuantityDto to QuantityEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedQuantityForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (QuantityException) e;
        }
        log.debug("Comparatively copied patched attributes from QuantityDto to QuantityEntity");

        log.debug("Saving patched QuantityEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched QuantityEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete QuantityEntity with id:{}", id);
            throw new QuantityException(InventoryErrorCode.INVENTORY_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch weight quantity details with id:" + id });
        }
        log.info("Patched QuantityEntity with id:{}", id);
    }

}