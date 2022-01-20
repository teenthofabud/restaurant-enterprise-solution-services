package com.teenthofabud.restaurant.solution.cookbook.ingredient.service.impl;

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
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientDto;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientEntity;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientException;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientForm;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientMessageTemplate;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.validator.ProductIdValidator;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeException;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.service.RecipeService;
import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.converter.IngredientDto2EntityConverter;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.converter.IngredientForm2EntityConverter;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.mapper.IngredientEntitySelfMapper;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.mapper.IngredientForm2EntityMapper;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.repository.IngredientRepository;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.service.IngredientService;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.validator.IngredientDtoValidator;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.validator.IngredientFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.validator.IngredientFormValidator;
import com.teenthofabud.restaurant.solution.cookbook.utils.CookbookServiceHelper;
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
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

@Component
@Slf4j
public class IngredientServiceImpl implements IngredientService {

    private static final Comparator<IngredientVo> CMP_BY_NAME_AND_INGREDIENT_ID_AND_PRODUCT_ID = (s1, s2) -> {
        return Integer.compare(Integer.compare(s1.getName().compareTo(s2.getName()), s1.getRecipeId().compareTo(s2.getRecipeId())),
                s1.getProductId().compareTo(s2.getProductId()));
    };

    private IngredientForm2EntityConverter form2EntityConverter;
    private IngredientDto2EntityConverter dto2EntityConverter;
    private IngredientForm2EntityMapper form2EntityMapper;
    private IngredientEntitySelfMapper entitySelfMapper;
    private IngredientFormValidator formValidator;
    private IngredientFormRelaxedValidator relaxedFormValidator;
    private IngredientDtoValidator dtoValidator;
    private IngredientRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private RecipeService recipeService;
    private ProductIdValidator productIdValidator;
    private CookbookServiceHelper cookbookServiceHelper;

    @Autowired
    public void setCookbookServiceHelper(CookbookServiceHelper cookbookServiceHelper) {
        this.cookbookServiceHelper = cookbookServiceHelper;
    }

    @Autowired
    public void setProductIdValidator(ProductIdValidator productIdValidator) {
        this.productIdValidator = productIdValidator;
    }

    @Autowired
    public void setRecipeService(RecipeService recipeService) {
        this.recipeService = recipeService;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setDto2EntityConverter(IngredientDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(IngredientForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(IngredientEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(IngredientFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchIngredientValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(IngredientDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(IngredientForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(IngredientRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(IngredientFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseIngredientId(String id) throws IngredientException {
        Long ingredientId = null;
        try {
            ingredientId = Long.parseLong(id);
            log.debug("Parsed id {} to ingredient id {} in numeric format", id, ingredientId);
            if(ingredientId <= 0) {
                throw new NumberFormatException("ingredient id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse ingredient id", e);
            log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_ID_INVALID.getValue(), id);
            throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return ingredientId;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public Set<IngredientVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all IngredientEntity by their natural ordering");
        List<IngredientEntity> ingredientEntityList = repository.findAll();
        Set<IngredientVo> naturallyOrderedSet = new TreeSet<IngredientVo>(CMP_BY_NAME_AND_INGREDIENT_ID_AND_PRODUCT_ID);
        for(IngredientEntity entity : ingredientEntityList) {
            List<IngredientVo> dtoList = cookbookServiceHelper.ingredientEntity2DetailedVo(List.of(entity));
            if(dtoList != null && !dtoList.isEmpty()) {
                IngredientVo dto = dtoList.get(0);
                log.debug("Converting {} to {}", entity, dto);
                naturallyOrderedSet.add(dto);
            }
        }
        log.info("{} IngredientVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public IngredientVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws IngredientException {
        log.info("Requesting IngredientEntity by id: {}", id);
        Long ingredientId = parseIngredientId(id);
        Optional<IngredientEntity> optEntity = repository.findById(ingredientId);
        if(optEntity.isEmpty()) {
            log.debug("No IngredientEntity found by id: {}", id);
            throw new IngredientException(CookbookErrorCode.COOK_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found IngredientVo by id: {}", id);
        IngredientEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        IngredientVo vo = cookbookServiceHelper.ingredientEntity2DetailedVo(entity);
        log.debug("IngredientVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<IngredientVo> retrieveAllMatchingDetailsByRecipeId(String recipeId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws IngredientException {
        log.info("Requesting IngredientEntity that match with recipeId: {}", recipeId);
        Errors err = new DirectFieldBindingResult(recipeId, "IngredientForm");
        try {
            RecipeVo recipeVo = recipeService.retrieveDetailsById(recipeId, Optional.of(TOABCascadeLevel.ONE));
            if(!recipeVo.getActive()) {
                throw new RecipeException(CookbookErrorCode.COOK_INACTIVE, new Object [] { recipeId });
            }
        } catch (RecipeException e) {
            log.error("recipeId is invalid", e);
            throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object [] { "recipeId: " + recipeId });
        }
        List<IngredientEntity> ingredientEntityList = repository.findByRecipeId(Long.parseLong(recipeId));
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        List<IngredientVo> matchedIngredientList = cookbookServiceHelper.ingredientEntity2DetailedVo(ingredientEntityList);
        log.info("Found {} IngredientVo matching with recipeId: {}", matchedIngredientList.size(),recipeId);
        TOABRequestContextHolder.clearCascadeLevelContext();
        if(matchedIngredientList.isEmpty()) {
            log.debug("No IngredientVo found matching with recipeId: {}", recipeId);
            throw new IngredientException(CookbookErrorCode.COOK_NOT_FOUND, new Object[] { "recipeId", recipeId });
        }
        return matchedIngredientList;
    }

    @Override
    public List<IngredientVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName, Optional<String> optionalDescription,
                                                                   Optional<String> optionalProductId, Optional<String> optionalQuantityAmount,
                                                                   Optional<String> optionalQuantityUnitId)
            throws IngredientException {
        if(optionalName.isEmpty() && optionalDescription.isEmpty() && optionalProductId.isEmpty()
                && optionalProductId.isEmpty() && optionalQuantityAmount.isEmpty() && optionalQuantityUnitId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String description = optionalDescription.isPresent() ? optionalDescription.get() : "";
        String productId = optionalProductId.isPresent() ? optionalProductId.get() : "";
        String quantityAmount = optionalQuantityAmount.isPresent() ? optionalQuantityAmount.get() : "";
        String quantityUnitId = optionalQuantityUnitId.isPresent() ? optionalQuantityUnitId.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(description))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(productId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(quantityAmount))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(quantityUnitId))) {
            log.debug("All search parameters are empty");
        }
        List<IngredientVo> matchedIngredientList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        IngredientEntity entity = new IngredientEntity();
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
        if(StringUtils.hasText(StringUtils.trimWhitespace(productId))) {
            Errors err = new DirectFieldBindingResult(productId, "filters");
            productIdValidator.validate(productId, err);
            if(err.hasErrors()) {
                log.error("productId is invalid: {}", err);
                log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_PRODUCT_ID_INVALID.getValue(), productId);
                throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "productId", productId });
            }
            log.debug("productId {} is valid", productId);
            providedFilters.put("productId", productId);
            entity.setProductId(productId);
            matcherCriteria = matcherCriteria.withMatcher("productId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(quantityAmount))) {
            Double qa = null;
            try {
                qa = Double.parseDouble(quantityAmount);
                if(qa <= 0) {
                    throw new NumberFormatException("invalid quantity amount: " + qa);
                }
            } catch (NumberFormatException e) {
                log.error("quantityAmount is invalid: {}", e);
                log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_QUANTITY_AMOUNT_INVALID.getValue(), quantityAmount);
                throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "quantityAmount", quantityAmount });
            }
            log.debug("quantityAmount {} is valid", quantityAmount);
            providedFilters.put("quantityAmount", quantityAmount);
            entity.setQuantityAmount(qa);
            matcherCriteria = matcherCriteria.withMatcher("quantityAmount", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(quantityUnitId))) {
            if(!cookbookServiceHelper.isWeightCodeValid(quantityUnitId)) {
                log.error("quantityUnitId is invalid");
                log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_QUANTITY_UNIT_ID_INVALID.getValue(), quantityUnitId);
                throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "quantityUnitId", quantityUnitId });

            }
            log.debug("quantityAmount {} is valid", quantityAmount);
            providedFilters.put("quantityUnitId", quantityUnitId);
            entity.setQuantityUnitId(quantityUnitId);
            matcherCriteria = matcherCriteria.withMatcher("quantityUnitId", match -> match.exact());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<IngredientEntity> ingredientEntityExample = Example.of(entity, matcherCriteria);
        List<IngredientEntity> ingredientEntityList = repository.findAll(ingredientEntityExample);
        if(ingredientEntityList != null && !ingredientEntityList.isEmpty()) {
            matchedIngredientList = cookbookServiceHelper.ingredientEntity2DetailedVo(ingredientEntityList);
            log.info("Found {} IngredientVo matching with provided parameters : {}", matchedIngredientList.size(), providedFilters);
        }
        log.info("No IngredientVo available matching with provided parameters : {}", matchedIngredientList.size(), providedFilters);
        return matchedIngredientList;
    }

    @Transactional
    @Override
    public String createIngredient(IngredientForm form) throws IngredientException {
        log.info("Creating new IngredientEntity");

        if(form == null) {
            log.debug("IngredientForm provided is null");
            throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of IngredientForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("IngredientForm has {} errors", err.getErrorCount());
            CookbookErrorCode ec = CookbookErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("IngredientForm error detail: {}", ec);
            throw new IngredientException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of IngredientForm are valid");

        log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_EXISTENCE_BY_NAME_AND_RECIPE_ID_AND_PRODUCT_ID.getValue(), form.getName(), form.getRecipeId(), form.getProductId());
        if(repository.existsByNameAndRecipeIdAndProductId(form.getName(), Long.parseLong(form.getRecipeId()), form.getProductId())) {
            log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_EXISTS_BY_NAME_AND_RECIPE_ID_AND_PRODUCT_ID.getValue(), form.getName(), form.getRecipeId(), form.getProductId());
            throw new IngredientException(CookbookErrorCode.COOK_EXISTS,
                    new Object[]{ "name: " + form.getName(), "recipeId: " + form.getRecipeId() + ", productId: " + form.getProductId() });
        }
        log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_NON_EXISTENCE_BY_NAME_AND_RECIPE_ID_AND_PRODUCT_ID.getValue(),
                form.getName(), form.getRecipeId(), form.getProductId());

        IngredientEntity expectedEntity = form2EntityConverter.convert(form);

        log.debug("Saving {}", expectedEntity);
        IngredientEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new IngredientException(CookbookErrorCode.COOK_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist IngredientForm details" });
        }
        log.info("Created new IngredientForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Transactional
    @Override
    public void updateIngredient(String id, IngredientForm form) throws IngredientException {
        log.info("Updating IngredientForm by id: {}", id);

        log.debug(IngredientMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_INGREDIENT_ENTITY_ID.getValue(), id);
        Long ingredientId = parseIngredientId(id);
        Optional<IngredientEntity> optActualEntity = repository.findById(ingredientId);
        if(optActualEntity.isEmpty()) {
            log.debug(IngredientMessageTemplate.MSG_TEMPLATE_NO_INGREDIENT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new IngredientException(CookbookErrorCode.COOK_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(IngredientMessageTemplate.MSG_TEMPLATE_FOUND_INGREDIENT_ENTITY_ID.getValue(), id);

        IngredientEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("IngredientEntity is inactive with id: {}", id);
            throw new IngredientException(CookbookErrorCode.COOK_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("IngredientEntity is active with id: {}", id);

        if(form == null) {
            log.debug("IngredientForm is null");
            throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of IngredientForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("IngredientForm has {} errors", err.getErrorCount());
            CookbookErrorCode ec = CookbookErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("IngredientForm error detail: {}", ec);
            throw new IngredientException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of IngredientForm are empty");
            throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of IngredientForm are valid");

        Optional<IngredientEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of IngredientForm");
            throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from IngredientForm to IngredientEntity");

        IngredientEntity expectedEntity = optExpectedEntity.get();

        checkUniquenessOfIngredient(form, actualEntity);

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from IngredientEntity to IngredientForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new IngredientException(CookbookErrorCode.COOK_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency ingredient details" });
        }
        log.info("Updated existing IngredientEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void deleteIngredient(String id) throws IngredientException {
        log.info("Soft deleting IngredientEntity by id: {}", id);

        log.debug(IngredientMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_INGREDIENT_ENTITY_ID.getValue(), id);
        Long ingredientId = parseIngredientId(id);
        Optional<IngredientEntity> optEntity = repository.findById(ingredientId);
        if(optEntity.isEmpty()) {
            log.debug(IngredientMessageTemplate.MSG_TEMPLATE_NO_INGREDIENT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new IngredientException(CookbookErrorCode.COOK_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(IngredientMessageTemplate.MSG_TEMPLATE_FOUND_INGREDIENT_ENTITY_ID.getValue(), id);

        IngredientEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("IngredientEntity is inactive with id: {}", id);
            throw new IngredientException(CookbookErrorCode.COOK_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("IngredientEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        IngredientEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new IngredientException(CookbookErrorCode.COOK_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current ingredient details with id:" + id });
        }

        log.info("Soft deleted existing IngredientEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void applyPatchOnIngredient(String id, List<PatchOperationForm> patches) throws IngredientException {
        log.info("Patching IngredientEntity by id: {}", id);

        log.debug(IngredientMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_INGREDIENT_ENTITY_ID.getValue(), id);
        Long ingredientId = parseIngredientId(id);
        Optional<IngredientEntity> optActualEntity = repository.findById(ingredientId);
        if(optActualEntity.isEmpty()) {
            log.debug(IngredientMessageTemplate.MSG_TEMPLATE_NO_INGREDIENT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new IngredientException(CookbookErrorCode.COOK_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(IngredientMessageTemplate.MSG_TEMPLATE_FOUND_INGREDIENT_ENTITY_ID.getValue(), id);

        IngredientEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Ingredient patch list not provided");
            throw new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Ingredient patch list has {} products", patches.size());


        log.debug("Validating patch list products for Ingredient");
        try {
            toabBaseService.validatePatches(patches, CookbookErrorCode.COOK_EXISTS.getDomain() + ":LOV");
            log.debug("All Ingredient patch list products are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Ingredient patch product are invalid");
            throw new IngredientException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list products for Ingredient");


        log.debug("Patching list products to IngredientDto");
        IngredientDto patchedIngredientForm = new IngredientDto();
        try {
            log.debug("Preparing patch list products for Ingredient");
            JsonNode ingredientDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch ingredientPatch = JsonPatch.fromJson(ingredientDtoTree);
            log.debug("Prepared patch list products for Ingredient");
            JsonNode blankIngredientDtoTree = om.convertValue(new IngredientDto(), JsonNode.class);
            JsonNode patchedIngredientFormTree = ingredientPatch.apply(blankIngredientDtoTree);
            log.debug("Applying patch list products to IngredientDto");
            patchedIngredientForm = om.treeToValue(patchedIngredientFormTree, IngredientDto.class);
            log.debug("Applied patch list products to IngredientDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list products to IngredientDto: {}", e);
            IngredientException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in IngredientDto");
                ex = new IngredientException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new IngredientException(CookbookErrorCode.COOK_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list products to IngredientDto: {}", e);
            throw new IngredientException(CookbookErrorCode.COOK_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list products to IngredientDto");

        log.debug("Validating patched IngredientDto");
        Errors err = new DirectFieldBindingResult(patchedIngredientForm, patchedIngredientForm.getClass().getSimpleName());
        dtoValidator.validate(patchedIngredientForm, err);
        if(err.hasErrors()) {
            log.debug("Patched IngredientDto has {} errors", err.getErrorCount());
            CookbookErrorCode ec = CookbookErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched IngredientDto error detail: {}", ec);
            throw new IngredientException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched IngredientDto are valid");

        checkUniquenessOfIngredient(patchedIngredientForm, actualEntity);

        log.debug("Comparatively copying patched attributes from IngredientDto to IngredientEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedIngredientForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (IngredientException) e;
        }
        log.debug("Comparatively copied patched attributes from IngredientDto to IngredientEntity");

        log.debug("Saving patched IngredientEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched IngredientEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete IngredientEntity with id:{}", id);
            throw new IngredientException(CookbookErrorCode.COOK_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency ingredient details with id:" + id });
        }
        log.info("Patched IngredientEntity with id:{}", id);
    }

    private void checkUniquenessOfIngredient(IngredientDto patchedIngredientForm, IngredientEntity actualEntity) throws IngredientException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(3);
        if(patchedIngredientForm.getName().isPresent()) {
            similaritySwitchesCollection.add(patchedIngredientForm.getName().get().compareTo(actualEntity.getName()) == 0);
        }
        if(patchedIngredientForm.getRecipeId().isPresent()) {
            similaritySwitchesCollection.add(patchedIngredientForm.getRecipeId().get().compareTo(actualEntity.getRecipe().getId().toString()) == 0);
        }
        if(patchedIngredientForm.getProductId().isPresent()) {
            similaritySwitchesCollection.add(patchedIngredientForm.getProductId().get().compareTo(actualEntity.getProductId()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String name = patchedIngredientForm.getName().isPresent() ? patchedIngredientForm.getName().get() : actualEntity.getName();
            String recipeId = patchedIngredientForm.getRecipeId().isPresent() ? patchedIngredientForm.getRecipeId().get() : actualEntity.getRecipe().getId().toString();
            String productId = patchedIngredientForm.getProductId().isPresent() ? patchedIngredientForm.getProductId().get() : actualEntity.getProductId();
            log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_EXISTENCE_BY_NAME_AND_RECIPE_ID_AND_PRODUCT_ID.getValue(), name, recipeId, productId);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  repository.existsByNameAndRecipeIdAndProductId(name, Long.parseLong(recipeId), productId);
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_EXISTS_BY_NAME_AND_RECIPE_ID_AND_PRODUCT_ID.getValue(), name, recipeId, productId);
                throw new IngredientException(CookbookErrorCode.COOK_EXISTS, new Object[]{ "name: " + name, ", recipeId: " + recipeId + ", productId: " + productId });
            }
            log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_NON_EXISTENCE_BY_NAME_AND_RECIPE_ID_AND_PRODUCT_ID.getValue(), name, recipeId, productId);

        }
    }

    private void checkUniquenessOfIngredient(IngredientForm ingredientForm, IngredientEntity actualEntity) throws IngredientException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(3);
        if(StringUtils.hasText(StringUtils.trimWhitespace(ingredientForm.getName()))) {
            similaritySwitchesCollection.add(ingredientForm.getName().compareTo(actualEntity.getName()) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(ingredientForm.getRecipeId()))) {
            similaritySwitchesCollection.add(ingredientForm.getRecipeId().compareTo(actualEntity.getRecipe().getId().toString()) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(ingredientForm.getProductId()))) {
            similaritySwitchesCollection.add(ingredientForm.getProductId().compareTo(actualEntity.getProductId()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String name = StringUtils.hasText(StringUtils.trimWhitespace(ingredientForm.getName())) ? ingredientForm.getName() : actualEntity.getName();
            String recipeId = StringUtils.hasText(StringUtils.trimWhitespace(ingredientForm.getRecipeId())) ? ingredientForm.getRecipeId() : actualEntity.getRecipe().getId().toString();
            String productId = StringUtils.hasText(StringUtils.trimWhitespace(ingredientForm.getProductId())) ? ingredientForm.getProductId() : actualEntity.getProductId();
            log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_EXISTENCE_BY_NAME_AND_RECIPE_ID_AND_PRODUCT_ID.getValue(), name, recipeId, productId);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  repository.existsByNameAndRecipeIdAndProductId(name, Long.parseLong(recipeId), productId);
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_EXISTS_BY_NAME_AND_RECIPE_ID_AND_PRODUCT_ID.getValue(), name, recipeId, productId);
                throw new IngredientException(CookbookErrorCode.COOK_EXISTS, new Object[]{ "name: " + name, ", recipeId: " + recipeId + ", productId: " + productId });
            }
            log.debug(IngredientMessageTemplate.MSG_TEMPLATE_INGREDIENT_NON_EXISTENCE_BY_NAME_AND_RECIPE_ID_AND_PRODUCT_ID.getValue(), name, recipeId, productId);

        }
    }
}