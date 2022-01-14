package com.teenthofabud.restaurant.solution.cookbook.recipe.service.impl;

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
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineException;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineVo;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.service.CuisineService;
import com.teenthofabud.restaurant.solution.cookbook.recipe.converter.RecipeDto2EntityConverter;
import com.teenthofabud.restaurant.solution.cookbook.recipe.converter.RecipeForm2EntityConverter;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeDto;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeException;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeForm;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeMessageTemplate;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.mapper.RecipeEntitySelfMapper;
import com.teenthofabud.restaurant.solution.cookbook.recipe.mapper.RecipeForm2EntityMapper;
import com.teenthofabud.restaurant.solution.cookbook.recipe.repository.RecipeRepository;
import com.teenthofabud.restaurant.solution.cookbook.recipe.service.RecipeService;
import com.teenthofabud.restaurant.solution.cookbook.recipe.validator.RecipeDtoValidator;
import com.teenthofabud.restaurant.solution.cookbook.recipe.validator.RecipeFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.cookbook.recipe.validator.RecipeFormValidator;
import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
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
import org.springframework.validation.Validator;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;

@Component
@Slf4j
public class RecipeServiceImpl implements RecipeService {

    private static final Comparator<RecipeVo> CMP_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID = (s1, s2) -> {
        return Integer.compare(s1.getCuisineId().compareTo(s2.getCuisineId()), s1.getName().compareTo(s2.getName()));
    };

    private RecipeForm2EntityConverter form2EntityConverter;
    private RecipeDto2EntityConverter dto2EntityConverter;
    private RecipeForm2EntityMapper form2EntityMapper;
    private RecipeEntitySelfMapper entitySelfMapper;
    private RecipeFormValidator formValidator;
    private RecipeFormRelaxedValidator relaxedFormValidator;
    private RecipeDtoValidator dtoValidator;
    private RecipeRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private CuisineService cuisineService;
    private Validator itemIdValidator;
    private CookbookServiceHelper cookbookServiceHelper;

    @Autowired
    public void setCookbookServiceHelper(CookbookServiceHelper cookbookServiceHelper) {
        this.cookbookServiceHelper = cookbookServiceHelper;
    }

    @Autowired
    public void setItemIdValidator(Validator itemIdValidator) {
        this.itemIdValidator = itemIdValidator;
    }

    @Autowired
    public void setCuisineService(CuisineService cuisineService) {
        this.cuisineService = cuisineService;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setDto2EntityConverter(RecipeDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(RecipeForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(RecipeEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(RecipeFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchRecipeValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(RecipeDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(RecipeForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(RecipeRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(RecipeFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseRecipeId(String id) throws RecipeException {
        Long recipeId = null;
        try {
            recipeId = Long.parseLong(id);
            log.debug("Parsed id {} to recipe id {} in numeric format", id, recipeId);
            if(recipeId <= 0) {
                throw new NumberFormatException("recipe id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse recipe id", e);
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_ID_INVALID.getValue(), id);
            throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return recipeId;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public Set<RecipeVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all RecipeEntity by their natural ordering");
        List<RecipeEntity> recipeEntityList = repository.findAll();
        Set<RecipeVo> naturallyOrderedSet = new TreeSet<RecipeVo>(CMP_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID);
        for(RecipeEntity entity : recipeEntityList) {
            List<RecipeVo> dtoList = cookbookServiceHelper.recipeEntity2DetailedVo(List.of(entity));
            if(dtoList != null && !dtoList.isEmpty()) {
                RecipeVo dto = dtoList.get(0);
                log.debug("Converting {} to {}", entity, dto);
                naturallyOrderedSet.add(dto);
            }
        }
        log.info("{} RecipeVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public RecipeVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws RecipeException {
        log.info("Requesting RecipeEntity by id: {}", id);
        Long recipeId = parseRecipeId(id);
        Optional<RecipeEntity> optEntity = repository.findById(recipeId);
        if(optEntity.isEmpty()) {
            log.debug("No RecipeEntity found by id: {}", id);
            throw new RecipeException(CookbookErrorCode.COOK_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found RecipeVo by id: {}", id);
        RecipeEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        RecipeVo vo = cookbookServiceHelper.recipeEntity2DetailedVo(entity);
        log.debug("RecipeVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<RecipeVo> retrieveAllMatchingDetailsByCuisineId(String cuisineId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws RecipeException {
        log.info("Requesting RecipeEntity that match with cuisineId: {}", cuisineId);
        Errors err = new DirectFieldBindingResult(cuisineId, "RecipeForm");
        try {
            CuisineVo cuisineVo = cuisineService.retrieveDetailsById(cuisineId, Optional.of(TOABCascadeLevel.ONE));
            if(!cuisineVo.getActive()) {
                throw new CuisineException(CookbookErrorCode.COOK_INACTIVE, new Object [] { cuisineId });
            }
        } catch (CuisineException e) {
            log.error("cuisineId is invalid", e);
            throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object [] { "cuisineId: " + cuisineId });
        }
        List<RecipeEntity> recipeEntityList = repository.findByCuisineId(Long.parseLong(cuisineId));
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        List<RecipeVo> matchedRecipeList = cookbookServiceHelper.recipeEntity2DetailedVo(recipeEntityList);
        log.info("Found {} RecipeVo matching with cuisineId: {}", matchedRecipeList.size(),cuisineId);
        TOABRequestContextHolder.clearCascadeLevelContext();
        if(matchedRecipeList.isEmpty()) {
            log.debug("No RecipeVo found matching with cuisineId: {}", cuisineId);
            throw new RecipeException(CookbookErrorCode.COOK_NOT_FOUND, new Object[] { "cuisineId", cuisineId });
        }
        return matchedRecipeList;
    }

    @Override
    public List<RecipeVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName,
                                                               Optional<String> optionalDescription,
                                                               Optional<String> optionalInstructions,
                                                               Optional<String> optionalCookingMethod,
                                                               Optional<String> optionalItemId,
                                                               Optional<String> optionalNumberOfServings,
                                                               Optional<String> optionalPreparationTimeDuration,
                                                               Optional<String> optionalPreparationTimeUnitId,
                                                               Optional<String> optionalCookingTimeDuration,
                                                               Optional<String> optionalCookingTimeUnitId,
                                                               Optional<String> optionalPortionSizeAmount,
                                                               Optional<String> optionalPortionSizeUnitId)
            throws RecipeException {
        if(optionalName.isEmpty() && optionalDescription.isEmpty() && optionalInstructions.isEmpty()
                && optionalCookingMethod.isEmpty() && optionalItemId.isEmpty() && optionalNumberOfServings.isEmpty()
                && optionalPreparationTimeDuration.isEmpty() && optionalPreparationTimeUnitId.isEmpty()
                && optionalCookingTimeDuration.isEmpty() && optionalCookingTimeUnitId.isEmpty()
                && optionalPortionSizeAmount.isEmpty() && optionalPortionSizeUnitId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String description = optionalDescription.isPresent() ? optionalDescription.get() : "";
        String instructions = optionalInstructions.isPresent() ? optionalInstructions.get() : "";
        String cookingMethod = optionalCookingMethod.isPresent() ? optionalCookingMethod.get() : "";
        String numberOfServings = optionalNumberOfServings.isPresent() ? optionalNumberOfServings.get() : "";
        String itemId = optionalItemId.isPresent() ? optionalItemId.get() : "";
        String preparationTimeDuration = optionalPreparationTimeDuration.isPresent() ? optionalPreparationTimeDuration.get() : "";
        String preparationTimeUnitId = optionalPreparationTimeUnitId.isPresent() ? optionalPreparationTimeUnitId.get() : "";
        String cookingTimeDuration = optionalCookingTimeDuration.isPresent() ? optionalCookingTimeDuration.get() : "";
        String cookingTimeUnitId = optionalCookingTimeUnitId.isPresent() ? optionalCookingTimeUnitId.get() : "";
        String portionSizeAmount = optionalPortionSizeAmount.isPresent() ? optionalPortionSizeAmount.get() : "";
        String portionSizeUnitId = optionalPortionSizeUnitId.isPresent() ? optionalPortionSizeUnitId.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(description))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(instructions)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cookingMethod))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(itemId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(preparationTimeDuration))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(preparationTimeUnitId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cookingTimeDuration))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(cookingTimeUnitId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(portionSizeAmount))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(portionSizeUnitId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(numberOfServings))) {
            log.debug("All search parameters are empty");
        }
        List<RecipeVo> matchedRecipeList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        RecipeEntity entity = new RecipeEntity();
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
        if(StringUtils.hasText(StringUtils.trimWhitespace(instructions))) {
            log.debug("instructions {} is valid", instructions);
            providedFilters.put("instructions", instructions);
            entity.setInstructions(instructions);
            matcherCriteria = matcherCriteria.withMatcher("instructions", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(cookingMethod))) {
            log.debug("cookingMethod {} is valid", cookingMethod);
            providedFilters.put("cookingMethod", cookingMethod);
            entity.setCookingMethod(cookingMethod);
            matcherCriteria = matcherCriteria.withMatcher("cookingMethod", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(itemId))) {
            Errors err = new DirectFieldBindingResult(itemId, "filters");
            itemIdValidator.validate(itemId, err);
            if(err.hasErrors()) {
                log.error("itemId is invalid: {}", err);
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_ITEM_ID_INVALID.getValue(), itemId);
                throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "itemId", itemId });
            }
            log.debug("itemId {} is valid", itemId);
            providedFilters.put("itemId", itemId);
            entity.setItemId(itemId);
            matcherCriteria = matcherCriteria.withMatcher("itemId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(numberOfServings))) {
            Integer nos = null;
            try {
                nos = Integer.parseInt(numberOfServings);
            } catch (NumberFormatException e) {
                log.error("numberOfServings is invalid: {}", e);
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_PREPARATION_TIME_DURATION_INVALID.getValue(), numberOfServings);
                throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "numberOfServings", numberOfServings });
            }
            log.debug("numberOfServings {} is valid", numberOfServings);
            providedFilters.put("numberOfServings", numberOfServings);
            entity.setNumberOfServings(nos);
            matcherCriteria = matcherCriteria.withMatcher("numberOfServings", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(preparationTimeDuration))) {
            Double ptd = null;
            try {
                ptd = Double.parseDouble(preparationTimeDuration);
            } catch (NumberFormatException e) {
                log.error("preparationTimeDuration is invalid: {}", e);
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_PREPARATION_TIME_DURATION_INVALID.getValue(), preparationTimeDuration);
                throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "preparationTimeDuration", preparationTimeDuration });
            }
            log.debug("preparationTimeDuration {} is valid", preparationTimeDuration);
            providedFilters.put("preparationTimeDuration", preparationTimeDuration);
            entity.setPreparationTimeDuration(ptd);
            matcherCriteria = matcherCriteria.withMatcher("preparationTimeDuration", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(preparationTimeUnitId))) {
            if(!cookbookServiceHelper.isTimeIdValid(preparationTimeUnitId)) {
                log.error("preparationTimeUnitId is invalid");
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_PREPARATION_TIME_UNIT_ID_INVALID.getValue(), preparationTimeUnitId);
                throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "preparationTimeUnitId", preparationTimeUnitId });
            }
            log.debug("preparationTimeUnitId {} is valid", preparationTimeUnitId);
            providedFilters.put("preparationTimeUnitId", preparationTimeUnitId);
            entity.setPreparationTimeUnitId(preparationTimeUnitId);
            matcherCriteria = matcherCriteria.withMatcher("preparationTimeUnitId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(cookingTimeDuration))) {
            Double ctd = null;
            try {
                ctd = Double.parseDouble(cookingTimeDuration);
            } catch (NumberFormatException e) {
                log.error("preparationTimeDuration is invalid: {}", e);
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_COOKING_TIME_DURATION_INVALID.getValue(), cookingTimeDuration);
                throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "cookingTimeDuration", cookingTimeDuration });
            }
            log.debug("cookingTimeDuration {} is valid", cookingTimeDuration);
            providedFilters.put("cookingTimeDuration", cookingTimeDuration);
            entity.setCookingTimeDuration(ctd);
            matcherCriteria = matcherCriteria.withMatcher("cookingTimeDuration", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(cookingTimeUnitId))) {
            if(!cookbookServiceHelper.isTimeIdValid(cookingTimeUnitId)) {
                log.error("cookingTimeUnitId is invalid");
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_PREPARATION_TIME_UNIT_ID_INVALID.getValue(), cookingTimeUnitId);
                throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "cookingTimeUnitId", cookingTimeUnitId });
            }
            log.debug("cookingTimeUnitId {} is valid", cookingTimeUnitId);
            providedFilters.put("cookingTimeUnitId", cookingTimeUnitId);
            entity.setCookingTimeUnitId(cookingTimeUnitId);
            matcherCriteria = matcherCriteria.withMatcher("cookingTimeUnitId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(portionSizeAmount))) {
            Double psa = null;
            try {
                psa = Double.parseDouble(portionSizeAmount);
            } catch (NumberFormatException e) {
                log.error("portionSizeAmount is invalid: {}", e);
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_COOKING_TIME_DURATION_INVALID.getValue(), portionSizeAmount);
                throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "portionSizeAmount", portionSizeAmount });
            }
            log.debug("portionSizeAmount {} is valid", portionSizeAmount);
            providedFilters.put("portionSizeAmount", portionSizeAmount);
            entity.setPortionSizeAmount(psa);
            matcherCriteria = matcherCriteria.withMatcher("portionSizeAmount", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(portionSizeUnitId))) {
            if(!cookbookServiceHelper.isTimeIdValid(portionSizeUnitId)) {
                log.error("portionSizeUnitId is invalid");
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_PREPARATION_TIME_UNIT_ID_INVALID.getValue(), portionSizeUnitId);
                throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "portionSizeUnitId", portionSizeUnitId });
            }
            log.debug("portionSizeUnitId {} is valid", portionSizeUnitId);
            providedFilters.put("portionSizeUnitId", portionSizeUnitId);
            entity.setPortionSizeUnitId(portionSizeUnitId);
            matcherCriteria = matcherCriteria.withMatcher("portionSizeUnitId", match -> match.exact());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<RecipeEntity> recipeEntityExample = Example.of(entity, matcherCriteria);
        List<RecipeEntity> recipeEntityList = repository.findAll(recipeEntityExample);
        if(recipeEntityList != null && !recipeEntityList.isEmpty()) {
            matchedRecipeList = cookbookServiceHelper.recipeEntity2DetailedVo(recipeEntityList);
            log.info("Found {} RecipeVo matching with provided parameters : {}", matchedRecipeList.size(), providedFilters);
        }
        log.info("No RecipeVo available matching with provided parameters : {}", matchedRecipeList.size(), providedFilters);
        return matchedRecipeList;
    }

    @Transactional
    @Override
    public String createRecipe(RecipeForm form) throws RecipeException {
        log.info("Creating new RecipeEntity");

        if(form == null) {
            log.debug("RecipeForm provided is null");
            throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of RecipeForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("RecipeForm has {} errors", err.getErrorCount());
            CookbookErrorCode ec = CookbookErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("RecipeForm error detail: {}", ec);
            throw new RecipeException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of RecipeForm are valid");

        log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(), form.getName(), form.getCuisineId(), form.getItemId());
        if(repository.existsByNameAndCuisineIdAndItemId(form.getName(), Long.parseLong(form.getCuisineId()), form.getItemId())) {
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTS_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(), form.getName(), form.getCuisineId(), form.getItemId());
            throw new RecipeException(CookbookErrorCode.COOK_EXISTS,
                    new Object[]{ "name: " + form.getName(), "cuisineId: " + form.getCuisineId() + ", itemId: " + form.getItemId() });
        }
        log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_NON_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                form.getName(), form.getCuisineId(), form.getItemId());

        RecipeEntity expectedEntity = form2EntityConverter.convert(form);

        log.debug("Saving {}", expectedEntity);
        RecipeEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new RecipeException(CookbookErrorCode.COOK_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist RecipeForm details" });
        }
        log.info("Created new RecipeForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Transactional
    @Override
    public void updateRecipe(String id, RecipeForm form) throws RecipeException {
        log.info("Updating RecipeForm by id: {}", id);

        log.debug(RecipeMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_RECIPE_ENTITY_ID.getValue(), id);
        Long recipeId = parseRecipeId(id);
        Optional<RecipeEntity> optActualEntity = repository.findById(recipeId);
        if(optActualEntity.isEmpty()) {
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_NO_RECIPE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new RecipeException(CookbookErrorCode.COOK_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(RecipeMessageTemplate.MSG_TEMPLATE_FOUND_RECIPE_ENTITY_ID.getValue(), id);

        RecipeEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("RecipeEntity is inactive with id: {}", id);
            throw new RecipeException(CookbookErrorCode.COOK_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("RecipeEntity is active with id: {}", id);

        if(form == null) {
            log.debug("RecipeForm is null");
            throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of RecipeForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("RecipeForm has {} errors", err.getErrorCount());
            CookbookErrorCode ec = CookbookErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("RecipeForm error detail: {}", ec);
            throw new RecipeException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of RecipeForm are empty");
            throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of RecipeForm are valid");

        Optional<RecipeEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of RecipeForm");
            throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from RecipeForm to RecipeEntity");

        RecipeEntity expectedEntity = optExpectedEntity.get();

        checkUniquenessOfRecipe(form, actualEntity);

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from RecipeEntity to RecipeForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new RecipeException(CookbookErrorCode.COOK_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency recipe details" });
        }
        log.info("Updated existing RecipeEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void deleteRecipe(String id) throws RecipeException {
        log.info("Soft deleting RecipeEntity by id: {}", id);

        log.debug(RecipeMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_RECIPE_ENTITY_ID.getValue(), id);
        Long recipeId = parseRecipeId(id);
        Optional<RecipeEntity> optEntity = repository.findById(recipeId);
        if(optEntity.isEmpty()) {
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_NO_RECIPE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new RecipeException(CookbookErrorCode.COOK_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(RecipeMessageTemplate.MSG_TEMPLATE_FOUND_RECIPE_ENTITY_ID.getValue(), id);

        RecipeEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("RecipeEntity is inactive with id: {}", id);
            throw new RecipeException(CookbookErrorCode.COOK_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("RecipeEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        RecipeEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new RecipeException(CookbookErrorCode.COOK_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current recipe details with id:" + id });
        }

        log.info("Soft deleted existing RecipeEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void applyPatchOnRecipe(String id, List<PatchOperationForm> patches) throws RecipeException {
        log.info("Patching RecipeEntity by id: {}", id);

        log.debug(RecipeMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_RECIPE_ENTITY_ID.getValue(), id);
        Long recipeId = parseRecipeId(id);
        Optional<RecipeEntity> optActualEntity = repository.findById(recipeId);
        if(optActualEntity.isEmpty()) {
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_NO_RECIPE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new RecipeException(CookbookErrorCode.COOK_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(RecipeMessageTemplate.MSG_TEMPLATE_FOUND_RECIPE_ENTITY_ID.getValue(), id);

        RecipeEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Recipe patch list not provided");
            throw new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Recipe patch list has {} items", patches.size());


        log.debug("Validating patch list items for Recipe");
        try {
            toabBaseService.validatePatches(patches, CookbookErrorCode.COOK_EXISTS.getDomain() + ":LOV");
            log.debug("All Recipe patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Recipe patch item are invalid");
            throw new RecipeException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Recipe");


        log.debug("Patching list items to RecipeDto");
        RecipeDto patchedRecipeForm = new RecipeDto();
        try {
            log.debug("Preparing patch list items for Recipe");
            JsonNode recipeDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch recipePatch = JsonPatch.fromJson(recipeDtoTree);
            log.debug("Prepared patch list items for Recipe");
            JsonNode blankRecipeDtoTree = om.convertValue(new RecipeDto(), JsonNode.class);
            JsonNode patchedRecipeFormTree = recipePatch.apply(blankRecipeDtoTree);
            log.debug("Applying patch list items to RecipeDto");
            patchedRecipeForm = om.treeToValue(patchedRecipeFormTree, RecipeDto.class);
            log.debug("Applied patch list items to RecipeDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to RecipeDto: {}", e);
            RecipeException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in RecipeDto");
                ex = new RecipeException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new RecipeException(CookbookErrorCode.COOK_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to RecipeDto: {}", e);
            throw new RecipeException(CookbookErrorCode.COOK_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to RecipeDto");

        log.debug("Validating patched RecipeDto");
        Errors err = new DirectFieldBindingResult(patchedRecipeForm, patchedRecipeForm.getClass().getSimpleName());
        dtoValidator.validate(patchedRecipeForm, err);
        if(err.hasErrors()) {
            log.debug("Patched RecipeDto has {} errors", err.getErrorCount());
            CookbookErrorCode ec = CookbookErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched RecipeDto error detail: {}", ec);
            throw new RecipeException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched RecipeDto are valid");

        checkUniquenessOfRecipe(patchedRecipeForm, actualEntity);

        log.debug("Comparatively copying patched attributes from RecipeDto to RecipeEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedRecipeForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (RecipeException) e;
        }
        log.debug("Comparatively copied patched attributes from RecipeDto to RecipeEntity");

        log.debug("Saving patched RecipeEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched RecipeEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete RecipeEntity with id:{}", id);
            throw new RecipeException(CookbookErrorCode.COOK_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency recipe details with id:" + id });
        }
        log.info("Patched RecipeEntity with id:{}", id);
    }

    private void checkUniquenessOfRecipe(RecipeDto patchedRecipeForm, RecipeEntity actualEntity) throws RecipeException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(3);
        if(patchedRecipeForm.getName().isPresent()) {
            similaritySwitchesCollection.add(patchedRecipeForm.getName().get().compareTo(actualEntity.getName()) == 0);
        }
        if(patchedRecipeForm.getCuisineId().isPresent()) {
            similaritySwitchesCollection.add(patchedRecipeForm.getCuisineId().get().compareTo(actualEntity.getCuisine().getId().toString()) == 0);
        }
        if(patchedRecipeForm.getItemId().isPresent()) {
            similaritySwitchesCollection.add(patchedRecipeForm.getItemId().get().compareTo(actualEntity.getItemId()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String name = patchedRecipeForm.getName().isPresent() ? patchedRecipeForm.getName().get() : actualEntity.getName();
            String cuisineId = patchedRecipeForm.getCuisineId().isPresent() ? patchedRecipeForm.getCuisineId().get() : actualEntity.getCuisine().getId().toString();
            String itemId = patchedRecipeForm.getItemId().isPresent() ? patchedRecipeForm.getItemId().get() : actualEntity.getItemId();
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(), name, cuisineId, itemId);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  repository.existsByNameAndCuisineIdAndItemId(name, Long.parseLong(cuisineId), itemId);
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTS_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(), name, cuisineId, itemId);
                throw new RecipeException(CookbookErrorCode.COOK_EXISTS, new Object[]{ "name: " + name, ", cuisineId: " + cuisineId + ", itemId: " + itemId });
            }
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_NON_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(), name, cuisineId, itemId);

        }
    }

    private void checkUniquenessOfRecipe(RecipeForm recipeForm, RecipeEntity actualEntity) throws RecipeException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(3);
        if(StringUtils.hasText(StringUtils.trimWhitespace(recipeForm.getName()))) {
            similaritySwitchesCollection.add(recipeForm.getName().compareTo(actualEntity.getName()) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(recipeForm.getCuisineId()))) {
            similaritySwitchesCollection.add(recipeForm.getCuisineId().compareTo(actualEntity.getCuisine().getId().toString()) == 0);
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(recipeForm.getItemId()))) {
            similaritySwitchesCollection.add(recipeForm.getItemId().compareTo(actualEntity.getItemId()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String name = StringUtils.hasText(StringUtils.trimWhitespace(recipeForm.getName())) ? recipeForm.getName() : actualEntity.getName();
            String cuisineId = StringUtils.hasText(StringUtils.trimWhitespace(recipeForm.getCuisineId())) ? recipeForm.getCuisineId() : actualEntity.getCuisine().getId().toString();
            String itemId = StringUtils.hasText(StringUtils.trimWhitespace(recipeForm.getItemId())) ? recipeForm.getItemId() : actualEntity.getItemId();
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(), name, cuisineId, itemId);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  repository.existsByNameAndCuisineIdAndItemId(name, Long.parseLong(cuisineId), itemId);
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTS_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(), name, cuisineId, itemId);
                throw new RecipeException(CookbookErrorCode.COOK_EXISTS, new Object[]{ "name: " + name, ", cuisineId: " + cuisineId + ", itemId: " + itemId });
            }
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_NON_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(), name, cuisineId, itemId);

        }
    }

    /*private void checkUniquenessOfRecipe(RecipeDto patchedRecipeForm, RecipeEntity actualEntity) throws RecipeException {
        // name = true, cuisineId = true, itemId = true
        if(patchedRecipeForm.getName().isPresent() && patchedRecipeForm.getCuisineId().isPresent() && patchedRecipeForm.getItemId().isPresent()) {
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    patchedRecipeForm.getName().get(), patchedRecipeForm.getCuisineId().get(), patchedRecipeForm.getItemId().get());
            boolean sameEntitySw = patchedRecipeForm.getName().get().compareTo(actualEntity.getName()) == 0
                    && patchedRecipeForm.getCuisineId().get().compareTo(actualEntity.getCuisine().getId().toString()) == 0
                    && patchedRecipeForm.getItemId().get().compareTo(actualEntity.getItemId()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCuisineIdAndItemId(patchedRecipeForm.getName().get(),
                    Long.parseLong(patchedRecipeForm.getCuisineId().get()), patchedRecipeForm.getItemId().get());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTS_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                        patchedRecipeForm.getName().get(), patchedRecipeForm.getCuisineId().get(), patchedRecipeForm.getItemId().get());
                throw new RecipeException(CookbookErrorCode.COOK_EXISTS,
                        new Object[]{ "name: " + patchedRecipeForm.getName().get(),
                                ", cuisineId: " + patchedRecipeForm.getCuisineId().get() + ", itemId: " + patchedRecipeForm.getItemId().get() });
            }
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_NON_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    patchedRecipeForm.getName().get(), patchedRecipeForm.getCuisineId().get(), patchedRecipeForm.getItemId().get());
        }

        // name = true, cuisineId = true, itemId = false
        if(patchedRecipeForm.getName().isPresent() && patchedRecipeForm.getCuisineId().isPresent() && patchedRecipeForm.getItemId().isEmpty()) {
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    patchedRecipeForm.getName().get(), patchedRecipeForm.getCuisineId().get(), patchedRecipeForm.getItemId().get());
            boolean sameEntitySw = patchedRecipeForm.getName().get().compareTo(actualEntity.getName()) == 0
                    && patchedRecipeForm.getCuisineId().get().compareTo(actualEntity.getCuisine().getId().toString()) == 0
                    && patchedRecipeForm.getItemId().get().compareTo(actualEntity.getItemId()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCuisineIdAndItemId(patchedRecipeForm.getName().get(),
                    Long.parseLong(patchedRecipeForm.getCuisineId().get()), patchedRecipeForm.getItemId().get());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTS_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                        patchedRecipeForm.getName().get(), patchedRecipeForm.getCuisineId().get(), patchedRecipeForm.getItemId().get());
                throw new RecipeException(CookbookErrorCode.COOK_EXISTS,
                        new Object[]{ "name: " + patchedRecipeForm.getName().get(),
                                ", cuisineId: " + patchedRecipeForm.getCuisineId().get() + ", itemId: " + patchedRecipeForm.getItemId().get() });
            }
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_NON_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    patchedRecipeForm.getName().get(), patchedRecipeForm.getCuisineId().get(), patchedRecipeForm.getItemId().get());
        }






















        // name = true, cuisineId = false, itemId = false
        if(patchedRecipeForm.getName().isPresent() && patchedRecipeForm.getCuisineId().isEmpty()) {
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    patchedRecipeForm.getName().get(), actualEntity.getCuisine().getId());
            boolean sameEntitySw = patchedRecipeForm.getName().get().compareTo(actualEntity.getName()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCuisineId(patchedRecipeForm.getName().get(), actualEntity.getCuisine().getId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTS_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                        patchedRecipeForm.getName().get(), actualEntity.getCuisine().getId());
                throw new RecipeException(CookbookErrorCode.COOK_EXISTS,
                        new Object[]{ "name: " + patchedRecipeForm.getName().get(), ", cuisineId: " + actualEntity.getCuisine().getId() });
            }
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_NON_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    patchedRecipeForm.getName().get(), actualEntity.getCuisine().getId());
        }

        // name = true, cuisineId = true, itemId = false
        if(patchedRecipeForm.getName().isPresent() && patchedRecipeForm.getCuisineId().isPresent()) {
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    patchedRecipeForm.getName().get(), patchedRecipeForm.getCuisineId().get());
            boolean sameEntitySw = patchedRecipeForm.getName().get().compareTo(actualEntity.getName().toString()) == 0
                    && patchedRecipeForm.getCuisineId().get().compareTo(actualEntity.getCuisine().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCuisineId(patchedRecipeForm.getName().get(),
                    Long.parseLong(patchedRecipeForm.getCuisineId().get()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTS_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                        patchedRecipeForm.getName().get(), patchedRecipeForm.getCuisineId().get());
                throw new RecipeException(CookbookErrorCode.COOK_EXISTS,
                        new Object[]{ "name: " + patchedRecipeForm.getName().get(), ", cuisineId: " + patchedRecipeForm.getCuisineId().get() });
            }
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_NON_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    patchedRecipeForm.getName().get(), patchedRecipeForm.getCuisineId().get());
        }

        // name = false, cuisineId = true, itemId = true
        if(patchedRecipeForm.getName().isEmpty() && patchedRecipeForm.getCuisineId().isPresent()) {
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    actualEntity.getName(),  patchedRecipeForm.getCuisineId().get());
            boolean sameEntitySw = patchedRecipeForm.getCuisineId().get().compareTo(actualEntity.getCuisine().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCuisineId(
                    actualEntity.getName(), Long.parseLong(patchedRecipeForm.getCuisineId().get()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTS_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                        actualEntity.getName(), patchedRecipeForm.getCuisineId().get());
                throw new RecipeException(CookbookErrorCode.COOK_EXISTS, new Object[]{ "name " + actualEntity.getName(),
                        ", cuisineId: " + patchedRecipeForm.getCuisineId().get() });
            }
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_NON_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    actualEntity.getName(),  patchedRecipeForm.getCuisineId().get());
        }
    }*/

    /*private void checkUniquenessOfRecipe(RecipeForm recipeForm, RecipeEntity actualEntity) throws RecipeException {
        // name = true, cuisineId = false
        if(StringUtils.hasText(StringUtils.trimWhitespace(recipeForm.getName()))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(recipeForm.getCuisineId()))) {
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    recipeForm.getName(), actualEntity.getCuisine().getId());
            boolean sameEntitySw = recipeForm.getName().compareTo(actualEntity.getName()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCuisineId(
                    recipeForm.getName(), actualEntity.getCuisine().getId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTS_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                        recipeForm.getName(), actualEntity.getCuisine().getId());
                throw new RecipeException(CookbookErrorCode.COOK_EXISTS,
                        new Object[]{ "name: " + recipeForm.getName(), ", cuisineId: " + actualEntity.getCuisine().getId() });
            }
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_NON_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    recipeForm.getName(), actualEntity.getCuisine().getId());
        }

        // name = true, cuisineId = true
        if(StringUtils.hasText(StringUtils.trimWhitespace(recipeForm.getName()))
                && StringUtils.hasText(StringUtils.trimWhitespace(recipeForm.getCuisineId()))) {
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    recipeForm.getName(), recipeForm.getCuisineId());
            boolean sameEntitySw = recipeForm.getName().compareTo(actualEntity.getName()) == 0
                    && recipeForm.getCuisineId().compareTo(actualEntity.getCuisine().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCuisineId(recipeForm.getName(), Long.parseLong(recipeForm.getCuisineId()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTS_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                        recipeForm.getName(), recipeForm.getCuisineId());
                throw new RecipeException(CookbookErrorCode.COOK_EXISTS,
                        new Object[]{ "cuisineId: " + recipeForm.getCuisineId(), ", name: " + recipeForm.getName() });
            }
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_NON_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    recipeForm.getName(), recipeForm.getCuisineId());
        }

        // name = false, cuisineId = true
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(recipeForm.getName()))
                && StringUtils.hasText(StringUtils.trimWhitespace(recipeForm.getCuisineId()))) {
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    actualEntity.getName(),  recipeForm.getCuisineId());
            boolean sameEntitySw = recipeForm.getName().compareTo(actualEntity.getName()) == 0
                    && recipeForm.getCuisineId().compareTo(actualEntity.getCuisine().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCuisineId(actualEntity.getName(), Long.parseLong(recipeForm.getCuisineId()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_EXISTS_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                        actualEntity.getName(),  recipeForm.getCuisineId());
                throw new RecipeException(CookbookErrorCode.COOK_EXISTS, new Object[]{ "name: " + actualEntity.getName(),
                        ", cuisineId: " + recipeForm.getCuisineId() });
            }
            log.debug(RecipeMessageTemplate.MSG_TEMPLATE_RECIPE_NON_EXISTENCE_BY_NAME_AND_CUISINE_ID_AND_ITEM_ID.getValue(),
                    actualEntity.getName(),  recipeForm.getCuisineId());
        }
    }*/

}